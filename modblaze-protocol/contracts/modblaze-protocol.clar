;; ModBlaze Protocol Cross-Chain Domain Name System

;; ---------------------------------------------------------------------------
;; Constants
;; ---------------------------------------------------------------------------

(define-constant CONTRACT-OWNER tx-sender)

(define-constant ERR-NOT-AUTHORIZED        (err u100))
(define-constant ERR-DOMAIN-TAKEN          (err u101))
(define-constant ERR-DOMAIN-NOT-FOUND      (err u102))
(define-constant ERR-SUBDOMAIN-TAKEN       (err u103))
(define-constant ERR-SUBDOMAIN-NOT-FOUND   (err u104))
(define-constant ERR-INSUFFICIENT-PAYMENT  (err u105))
(define-constant ERR-DOMAIN-EXPIRED        (err u106))
(define-constant ERR-NOT-OWNER             (err u107))
(define-constant ERR-INVALID-MULTISIG      (err u108))
(define-constant ERR-BRIDGE-EXISTS         (err u109))
(define-constant ERR-BRIDGE-NOT-FOUND      (err u110))
(define-constant ERR-ZERO-AMOUNT           (err u111))

;; Registration duration: ~1 year in Stacks blocks (144 blocks/day * 365)
(define-constant BLOCKS-PER-YEAR u52560)

;; Reward pool share for domain holders (in basis points, 100 = 1%)
(define-constant REWARD-BPS u500) ;; 5% of cross-chain fees go to domain holder

;; ---------------------------------------------------------------------------
;; Data Variables
;; ---------------------------------------------------------------------------

;; Base price per domain registration in uSTX
(define-data-var base-domain-price uint u1000000) ;; 1 STX

;; Accumulated protocol fee pool (uSTX)
(define-data-var fee-pool uint u0)

;; Total domains registered
(define-data-var total-domains uint u0)

;; ---------------------------------------------------------------------------
;; Data Maps - Base Layer (Domain Ownership)
;; ---------------------------------------------------------------------------

;; Primary domain registry
(define-map domains
  { name: (string-ascii 64) }
  {
    owner:       principal,
    resolver:    (optional principal), ;; optional custom resolver contract
    registered:  uint,                 ;; block height of registration
    expires:     uint,                 ;; block height of expiry
    reputation:  uint,                 ;; reputation score (0-1000) for consensus weighting
    rewards-claimed: uint              ;; total uSTX rewards claimed by owner
  }
)

;; Reverse lookup: principal -> list of owned domain names (max 20)
(define-map owner-domains
  principal
  (list 20 (string-ascii 64))
)

;; ---------------------------------------------------------------------------
;; Data Maps - Bridge Layer (Cross-Chain State Sync)
;; ---------------------------------------------------------------------------

;; Supported external chains indexed by chain-id (e.g., u1 = Ethereum, u56 = BSC)
(define-map bridge-records
  { name: (string-ascii 64), chain-id: uint }
  {
    foreign-address: (string-ascii 128), ;; address on the foreign chain
    zk-attestation:  (buff 64),          ;; ZK proof hash attesting the mapping
    synced-at:       uint,               ;; block height of last sync
    active:          bool
  }
)

;; ---------------------------------------------------------------------------
;; Data Maps - Application Layer (Subdomain Management)
;; ---------------------------------------------------------------------------

;; Subdomain registry under a parent domain
(define-map subdomains
  { parent: (string-ascii 64), sub: (string-ascii 32) }
  {
    owner:      principal,
    target:     (string-ascii 256), ;; resolution target (address, URL, IPFS hash, etc.)
    created:    uint,
    metadata:   (string-ascii 256)  ;; arbitrary dApp-specific metadata
  }
)

;; ---------------------------------------------------------------------------
;; Data Maps - Multi-Signature Transfer
;; ---------------------------------------------------------------------------

;; Pending transfer requests require approval from current owner + optionally a co-signer
(define-map transfer-requests
  { name: (string-ascii 64) }
  {
    new-owner:  principal,
    cosigner:   (optional principal),
    approved:   bool,
    requested-at: uint
  }
)

;; ---------------------------------------------------------------------------
;; Private Helpers
;; ---------------------------------------------------------------------------

;; Check if a domain exists and is not expired
(define-private (domain-active? (name (string-ascii 64)))
  (match (map-get? domains { name: name })
    entry (< block-height (get expires entry))
    false
  )
)

;; Compute dynamic price: base price + reputation discount
;; Higher-reputation domains get a modest discount (up to 20%)
(define-private (compute-price (reputation uint))
  (let (
    (base (var-get base-domain-price))
    (discount (/ (* base (if (> reputation u500) u200 u0)) u1000))
  )
    (- base discount)
  )
)

;; Add a domain name to an owner's list (no-op if already at capacity)
(define-private (add-to-owner-list (owner principal) (name (string-ascii 64)))
  (let ((current (default-to (list) (map-get? owner-domains owner))))
    (map-set owner-domains owner (unwrap-panic (as-max-len? (append current name) u20)))
  )
)

;; Compute reward share for a domain holder from the fee pool
(define-private (compute-reward (pool uint))
  (/ (* pool REWARD-BPS) u10000)
)

;; ---------------------------------------------------------------------------
;; Public Functions - Base Layer
;; ---------------------------------------------------------------------------

;; Register a new domain
(define-public (register-domain
    (name       (string-ascii 64))
    (resolver   (optional principal))
  )
  (let (
    (caller tx-sender)
    (price  (compute-price u0)) ;; new registrants have reputation 0
    (expiry (+ block-height BLOCKS-PER-YEAR))
  )
    ;; Domain must not already be active
    (asserts! (not (domain-active? name)) ERR-DOMAIN-TAKEN)
    ;; Collect registration fee
    (try! (stx-transfer? price caller (as-contract tx-sender)))
    ;; Credit a portion to fee pool
    (var-set fee-pool (+ (var-get fee-pool) (/ price u10)))
    ;; Write domain record
    (map-set domains { name: name }
      {
        owner:           caller,
        resolver:        resolver,
        registered:      block-height,
        expires:         expiry,
        reputation:      u0,
        rewards-claimed: u0
      }
    )
    (add-to-owner-list caller name)
    (var-set total-domains (+ (var-get total-domains) u1))
    (ok true)
  )
)

;; Renew a domain for another year (can be called by anyone on behalf of owner)
(define-public (renew-domain (name (string-ascii 64)))
  (let (
    (entry  (unwrap! (map-get? domains { name: name }) ERR-DOMAIN-NOT-FOUND))
    (caller tx-sender)
    (price  (compute-price (get reputation entry)))
    (new-expiry (+ (get expires entry) BLOCKS-PER-YEAR))
  )
    (try! (stx-transfer? price caller (as-contract tx-sender)))
    (var-set fee-pool (+ (var-get fee-pool) (/ price u10)))
    (map-set domains { name: name }
      (merge entry { expires: new-expiry })
    )
    (ok new-expiry)
  )
)

;; Update the resolver for a domain (owner only)
(define-public (set-resolver (name (string-ascii 64)) (resolver (optional principal)))
  (let ((entry (unwrap! (map-get? domains { name: name }) ERR-DOMAIN-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner entry)) ERR-NOT-OWNER)
    (asserts! (domain-active? name) ERR-DOMAIN-EXPIRED)
    (map-set domains { name: name } (merge entry { resolver: resolver }))
    (ok true)
  )
)

;; Initiate a multi-sig transfer (owner initiates, optionally requires a cosigner)
(define-public (initiate-transfer
    (name      (string-ascii 64))
    (new-owner principal)
    (cosigner  (optional principal))
  )
  (let ((entry (unwrap! (map-get? domains { name: name }) ERR-DOMAIN-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner entry)) ERR-NOT-OWNER)
    (asserts! (domain-active? name) ERR-DOMAIN-EXPIRED)
    (map-set transfer-requests { name: name }
      {
        new-owner:    new-owner,
        cosigner:     cosigner,
        approved:     (is-none cosigner), ;; auto-approved if no cosigner required
        requested-at: block-height
      }
    )
    (ok true)
  )
)

;; Cosigner approves a pending transfer
(define-public (approve-transfer (name (string-ascii 64)))
  (let (
    (req   (unwrap! (map-get? transfer-requests { name: name }) ERR-DOMAIN-NOT-FOUND))
    (entry (unwrap! (map-get? domains { name: name }) ERR-DOMAIN-NOT-FOUND))
  )
    (asserts! (is-eq (some tx-sender) (get cosigner req)) ERR-INVALID-MULTISIG)
    (map-set transfer-requests { name: name } (merge req { approved: true }))
    (ok true)
  )
)

;; Finalize transfer once approved
(define-public (finalize-transfer (name (string-ascii 64)))
  (let (
    (req   (unwrap! (map-get? transfer-requests { name: name }) ERR-DOMAIN-NOT-FOUND))
    (entry (unwrap! (map-get? domains { name: name }) ERR-DOMAIN-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get owner entry)) ERR-NOT-OWNER)
    (asserts! (get approved req) ERR-INVALID-MULTISIG)
    (asserts! (domain-active? name) ERR-DOMAIN-EXPIRED)
    (map-set domains { name: name } (merge entry { owner: (get new-owner req) }))
    (map-delete transfer-requests { name: name })
    (add-to-owner-list (get new-owner req) name)
    (ok true)
  )
)

;; ---------------------------------------------------------------------------
;; Public Functions - Bridge Layer
;; ---------------------------------------------------------------------------

;; Register or update a cross-chain address mapping with a ZK attestation hash
(define-public (set-bridge-record
    (name            (string-ascii 64))
    (chain-id        uint)
    (foreign-address (string-ascii 128))
    (zk-attestation  (buff 64))
  )
  (let ((entry (unwrap! (map-get? domains { name: name }) ERR-DOMAIN-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner entry)) ERR-NOT-OWNER)
    (asserts! (domain-active? name) ERR-DOMAIN-EXPIRED)
    (map-set bridge-records { name: name, chain-id: chain-id }
      {
        foreign-address: foreign-address,
        zk-attestation:  zk-attestation,
        synced-at:       block-height,
        active:          true
      }
    )
    (ok true)
  )
)

;; Deactivate a bridge record
(define-public (deactivate-bridge
    (name     (string-ascii 64))
    (chain-id uint)
  )
  (let (
    (entry  (unwrap! (map-get? domains { name: name }) ERR-DOMAIN-NOT-FOUND))
    (bridge (unwrap! (map-get? bridge-records { name: name, chain-id: chain-id }) ERR-BRIDGE-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get owner entry)) ERR-NOT-OWNER)
    (map-set bridge-records { name: name, chain-id: chain-id }
      (merge bridge { active: false })
    )
    (ok true)
  )
)

;; ---------------------------------------------------------------------------
;; Public Functions - Application Layer (Subdomains)
;; ---------------------------------------------------------------------------

;; Create a subdomain under a parent domain (parent owner only)
(define-public (create-subdomain
    (parent   (string-ascii 64))
    (sub      (string-ascii 32))
    (target   (string-ascii 256))
    (metadata (string-ascii 256))
  )
  (let ((entry (unwrap! (map-get? domains { name: parent }) ERR-DOMAIN-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner entry)) ERR-NOT-OWNER)
    (asserts! (domain-active? parent) ERR-DOMAIN-EXPIRED)
    (asserts! (is-none (map-get? subdomains { parent: parent, sub: sub })) ERR-SUBDOMAIN-TAKEN)
    (map-set subdomains { parent: parent, sub: sub }
      {
        owner:    tx-sender,
        target:   target,
        created:  block-height,
        metadata: metadata
      }
    )
    (ok true)
  )
)

;; Update a subdomain's target or metadata (parent owner only)
(define-public (update-subdomain
    (parent   (string-ascii 64))
    (sub      (string-ascii 32))
    (target   (string-ascii 256))
    (metadata (string-ascii 256))
  )
  (let (
    (entry    (unwrap! (map-get? domains { name: parent }) ERR-DOMAIN-NOT-FOUND))
    (sd-entry (unwrap! (map-get? subdomains { parent: parent, sub: sub }) ERR-SUBDOMAIN-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get owner entry)) ERR-NOT-OWNER)
    (asserts! (domain-active? parent) ERR-DOMAIN-EXPIRED)
    (map-set subdomains { parent: parent, sub: sub }
      (merge sd-entry { target: target, metadata: metadata })
    )
    (ok true)
  )
)

;; Delete a subdomain (parent owner only)
(define-public (delete-subdomain (parent (string-ascii 64)) (sub (string-ascii 32)))
  (let ((entry (unwrap! (map-get? domains { name: parent }) ERR-DOMAIN-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner entry)) ERR-NOT-OWNER)
    (map-delete subdomains { parent: parent, sub: sub })
    (ok true)
  )
)

;; ---------------------------------------------------------------------------
;; Public Functions - Rewards
;; ---------------------------------------------------------------------------

;; Deposit cross-chain transaction fees into the protocol pool (anyone can call)
(define-public (deposit-fees (amount uint))
  (begin
    (asserts! (> amount u0) ERR-ZERO-AMOUNT)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set fee-pool (+ (var-get fee-pool) amount))
    (ok true)
  )
)

;; Claim reward share for a domain owner
(define-public (claim-reward (name (string-ascii 64)))
  (let (
    (entry  (unwrap! (map-get? domains { name: name }) ERR-DOMAIN-NOT-FOUND))
    (caller tx-sender)
    (pool   (var-get fee-pool))
    (reward (compute-reward pool))
  )
    (asserts! (is-eq caller (get owner entry)) ERR-NOT-OWNER)
    (asserts! (domain-active? name) ERR-DOMAIN-EXPIRED)
    (asserts! (> reward u0) ERR-ZERO-AMOUNT)
    ;; Drain reward from pool
    (var-set fee-pool (- pool reward))
    ;; Transfer reward to owner
    (try! (as-contract (stx-transfer? reward tx-sender caller)))
    ;; Update claimed counter
    (map-set domains { name: name }
      (merge entry { rewards-claimed: (+ (get rewards-claimed entry) reward) })
    )
    (ok reward)
  )
)

;; ---------------------------------------------------------------------------
;; Admin Functions
;; ---------------------------------------------------------------------------

;; Update the base registration price (contract owner only)
(define-public (set-base-price (new-price uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (> new-price u0) ERR-ZERO-AMOUNT)
    (var-set base-domain-price new-price)
    (ok true)
  )
)

;; Boost a domain's reputation score (contract owner / governance; 0-1000 scale)
(define-public (set-reputation (name (string-ascii 64)) (score uint))
  (let ((entry (unwrap! (map-get? domains { name: name }) ERR-DOMAIN-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (<= score u1000) ERR-NOT-AUTHORIZED)
    (map-set domains { name: name } (merge entry { reputation: score }))
    (ok true)
  )
)

;; ---------------------------------------------------------------------------
;; Read-Only Functions
;; ---------------------------------------------------------------------------

;; Resolve a domain: returns owner, resolver, and expiry
(define-read-only (resolve-domain (name (string-ascii 64)))
  (map-get? domains { name: name })
)

;; Resolve a subdomain to its target
(define-read-only (resolve-subdomain (parent (string-ascii 64)) (sub (string-ascii 32)))
  (map-get? subdomains { parent: parent, sub: sub })
)

;; Look up a cross-chain bridge record
(define-read-only (resolve-bridge (name (string-ascii 64)) (chain-id uint))
  (map-get? bridge-records { name: name, chain-id: chain-id })
)

;; Check whether a domain is currently active (registered and not expired)
(define-read-only (is-active (name (string-ascii 64)))
  (domain-active? name)
)

;; Get list of domains owned by a principal
(define-read-only (get-owner-domains (owner principal))
  (default-to (list) (map-get? owner-domains owner))
)

;; Get current protocol fee pool balance
(define-read-only (get-fee-pool)
  (var-get fee-pool)
)

;; Get current base domain price
(define-read-only (get-base-price)
  (var-get base-domain-price)
)

;; Get total registered domains
(define-read-only (get-total-domains)
  (var-get total-domains)
)

;; Compute the registration price for a given reputation score
(define-read-only (get-price-for-reputation (reputation uint))
  (compute-price reputation)
)
