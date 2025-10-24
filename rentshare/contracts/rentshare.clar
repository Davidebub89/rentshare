;; Rent Share - Tokenized Property Rental Income Distribution
;; Owners tokenize properties, investors buy shares, automatic income distribution

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-input (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-property-inactive (err u105))
(define-constant err-no-shares (err u106))
(define-constant err-contract-paused (err u107))
(define-constant err-already-claimed (err u108))

;; Data Variables
(define-data-var next-property-id uint u1)
(define-data-var next-distribution-id uint u1)
(define-data-var platform-fee uint u500) ;; 5% in basis points
(define-data-var min-share-price uint u1000000) ;; 1 STX minimum
(define-data-var contract-paused bool false)

;; Data Maps
(define-map rental-properties
    { property-id: uint }
    {
        owner: principal,
        property-name: (string-ascii 64),
        property-address: (string-ascii 128),
        property-type: (string-ascii 32),
        total-shares: uint,
        shares-sold: uint,
        share-price: uint,
        monthly-rent: uint,
        total-distributions: uint,
        total-income: uint,
        is-active: bool,
        verification-status: (string-ascii 16),
        created-at: uint
    }
)

(define-map shareholder-positions
    { property-id: uint, shareholder: principal }
    {
        shares-owned: uint,
        total-invested: uint,
        total-received: uint,
        last-claim-distribution: uint,
        purchase-date: uint
    }
)

(define-map income-distributions
    { distribution-id: uint }
    {
        property-id: uint,
        distribution-amount: uint,
        distribution-per-share: uint,
        distribution-date: uint,
        total-claimed: uint,
        claim-deadline: uint
    }
)

(define-map distribution-claims
    { distribution-id: uint, shareholder: principal }
    {
        amount-claimed: uint,
        claim-date: uint
    }
)

(define-map property-documents
    { property-id: uint, document-type: (string-ascii 32) }
    {
        document-hash: (string-ascii 64),
        document-url: (string-ascii 128),
        upload-date: uint,
        verified: bool
    }
)

(define-map investor-profiles
    { investor: principal }
    {
        display-name: (string-ascii 32),
        properties-invested: uint,
        total-shares: uint,
        total-invested: uint,
        total-income-received: uint,
        join-date: uint
    }
)

(define-map property-stats
    { property-id: uint }
    {
        occupancy-rate: uint,
        avg-monthly-income: uint,
        total-shareholders: uint,
        roi-percentage: uint,
        last-distribution: uint
    }
)

;; Authorization Functions
(define-private (is-owner)
    (is-eq tx-sender contract-owner)
)

(define-private (is-property-owner (property-id uint))
    (match (map-get? rental-properties { property-id: property-id })
        property (is-eq tx-sender (get owner property))
        false
    )
)

(define-private (owns-shares (property-id uint) (shareholder principal))
    (match (map-get? shareholder-positions { property-id: property-id, shareholder: shareholder })
        position (> (get shares-owned position) u0)
        false
    )
)

;; Admin Functions
(define-public (set-platform-fee (new-fee uint))
    (begin
        (asserts! (is-owner) err-owner-only)
        (asserts! (<= new-fee u2000) err-invalid-input)
        (var-set platform-fee new-fee)
        (ok true)
    )
)

(define-public (verify-property (property-id uint))
    (begin
        (asserts! (is-owner) err-owner-only)
        (match (map-get? rental-properties { property-id: property-id })
            property (begin
                (map-set rental-properties
                    { property-id: property-id }
                    (merge property { verification-status: "verified" })
                )
                (ok true)
            )
            err-not-found
        )
    )
)

(define-public (pause-contract)
    (begin
        (asserts! (is-owner) err-owner-only)
        (var-set contract-paused true)
        (ok true)
    )
)

(define-public (unpause-contract)
    (begin
        (asserts! (is-owner) err-owner-only)
        (var-set contract-paused false)
        (ok true)
    )
)

;; Investor Profile Functions
(define-public (create-investor-profile (display-name (string-ascii 32)))
    (let
        ((existing-profile (map-get? investor-profiles { investor: tx-sender })))
        (begin
            (asserts! (not (var-get contract-paused)) err-contract-paused)
            (asserts! (is-none existing-profile) err-invalid-input)
            (asserts! (> (len display-name) u0) err-invalid-input)
            
            (map-set investor-profiles
                { investor: tx-sender }
                {
                    display-name: display-name,
                    properties-invested: u0,
                    total-shares: u0,
                    total-invested: u0,
                    total-income-received: u0,
                    join-date: u0
                }
            )
            (ok true)
        )
    )
)

;; Property Listing
(define-public (list-property
    (property-name (string-ascii 64))
    (property-address (string-ascii 128))
    (property-type (string-ascii 32))
    (total-shares uint)
    (share-price uint)
    (monthly-rent uint))
    (let
        (
            (property-id (var-get next-property-id))
        )
        (begin
            (asserts! (not (var-get contract-paused)) err-contract-paused)
            (asserts! (> (len property-name) u0) err-invalid-input)
            (asserts! (> total-shares u0) err-invalid-input)
            (asserts! (>= share-price (var-get min-share-price)) err-invalid-input)
            (asserts! (> monthly-rent u0) err-invalid-input)
            
            (map-set rental-properties
                { property-id: property-id }
                {
                    owner: tx-sender,
                    property-name: property-name,
                    property-address: property-address,
                    property-type: property-type,
                    total-shares: total-shares,
                    shares-sold: u0,
                    share-price: share-price,
                    monthly-rent: monthly-rent,
                    total-distributions: u0,
                    total-income: u0,
                    is-active: true,
                    verification-status: "pending",
                    created-at: u0
                }
            )
            
            ;; Initialize property stats
            (map-set property-stats
                { property-id: property-id }
                {
                    occupancy-rate: u100,
                    avg-monthly-income: monthly-rent,
                    total-shareholders: u0,
                    roi-percentage: u0,
                    last-distribution: u0
                }
            )
            
            ;; Increment property ID
            (var-set next-property-id (+ property-id u1))
            
            (ok property-id)
        )
    )
)

;; Share Purchase
(define-public (purchase-shares
    (property-id uint)
    (num-shares uint))
    (let
        (
            (property-data (unwrap! (map-get? rental-properties { property-id: property-id }) err-not-found))
            (total-cost (* num-shares (get share-price property-data)))
            (existing-position (map-get? shareholder-positions { property-id: property-id, shareholder: tx-sender }))
        )
        (begin
            (asserts! (not (var-get contract-paused)) err-contract-paused)
            (asserts! (get is-active property-data) err-property-inactive)
            (asserts! (> num-shares u0) err-invalid-input)
            (asserts! (<= (+ (get shares-sold property-data) num-shares) (get total-shares property-data)) err-insufficient-funds)
            
            ;; Transfer payment
            (try! (stx-transfer? total-cost tx-sender (get owner property-data)))
            
            ;; Update or create shareholder position
            (match existing-position
                position (map-set shareholder-positions
                    { property-id: property-id, shareholder: tx-sender }
                    {
                        shares-owned: (+ (get shares-owned position) num-shares),
                        total-invested: (+ (get total-invested position) total-cost),
                        total-received: (get total-received position),
                        last-claim-distribution: (get last-claim-distribution position),
                        purchase-date: (get purchase-date position)
                    }
                )
                (map-set shareholder-positions
                    { property-id: property-id, shareholder: tx-sender }
                    {
                        shares-owned: num-shares,
                        total-invested: total-cost,
                        total-received: u0,
                        last-claim-distribution: u0,
                        purchase-date: u0
                    }
                )
            )
            
            ;; Update property shares sold
            (map-set rental-properties
                { property-id: property-id }
                (merge property-data {
                    shares-sold: (+ (get shares-sold property-data) num-shares)
                })
            )
            
            ;; Update property stats
            (match (map-get? property-stats { property-id: property-id })
                stats (map-set property-stats
                    { property-id: property-id }
                    (merge stats {
                        total-shareholders: (if (is-none existing-position)
                            (+ (get total-shareholders stats) u1)
                            (get total-shareholders stats)
                        )
                    })
                )
                true
            )
            
            ;; Update investor profile
            (update-investor-stats tx-sender num-shares total-cost)
            
            (ok num-shares)
        )
    )
)

;; Income Distribution
(define-public (distribute-rental-income
    (property-id uint)
    (amount uint))
    (let
        (
            (property-data (unwrap! (map-get? rental-properties { property-id: property-id }) err-not-found))
            (distribution-id (var-get next-distribution-id))
            (platform-fee-amount (/ (* amount (var-get platform-fee)) u10000))
            (net-distribution (- amount platform-fee-amount))
            (shares-sold (get shares-sold property-data))
        )
        (begin
            (asserts! (not (var-get contract-paused)) err-contract-paused)
            (asserts! (is-property-owner property-id) err-unauthorized)
            (asserts! (> shares-sold u0) err-no-shares)
            (asserts! (> amount u0) err-invalid-input)
            
            ;; Transfer income to contract
            (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
            
            ;; Pay platform fee
            (try! (as-contract (stx-transfer? platform-fee-amount tx-sender contract-owner)))
            
            ;; Calculate per-share distribution
            (let
                ((per-share-amount (/ net-distribution shares-sold)))
                (begin
                    ;; Create distribution record
                    (map-set income-distributions
                        { distribution-id: distribution-id }
                        {
                            property-id: property-id,
                            distribution-amount: net-distribution,
                            distribution-per-share: per-share-amount,
                            distribution-date: u0,
                            total-claimed: u0,
                            claim-deadline: (+ u0 u4320) ;; ~30 days
                        }
                    )
                    
                    ;; Update property totals
                    (map-set rental-properties
                        { property-id: property-id }
                        (merge property-data {
                            total-distributions: (+ (get total-distributions property-data) u1),
                            total-income: (+ (get total-income property-data) net-distribution)
                        })
                    )
                    
                    ;; Update property stats
                    (match (map-get? property-stats { property-id: property-id })
                        stats (map-set property-stats
                            { property-id: property-id }
                            (merge stats {
                                last-distribution: distribution-id
                            })
                        )
                        true
                    )
                    
                    ;; Increment distribution ID
                    (var-set next-distribution-id (+ distribution-id u1))
                    
                    (ok distribution-id)
                )
            )
        )
    )
)

;; Claim Distribution
(define-public (claim-distribution (distribution-id uint))
    (let
        (
            (distribution-data (unwrap! (map-get? income-distributions { distribution-id: distribution-id }) err-not-found))
            (property-id (get property-id distribution-data))
            (shareholder-position (unwrap! (map-get? shareholder-positions { property-id: property-id, shareholder: tx-sender }) err-no-shares))
            (existing-claim (map-get? distribution-claims { distribution-id: distribution-id, shareholder: tx-sender }))
        )
        (begin
            (asserts! (not (var-get contract-paused)) err-contract-paused)
            (asserts! (is-none existing-claim) err-already-claimed)
            (asserts! (>= (get claim-deadline distribution-data) u0) err-invalid-input)
            
            ;; Calculate claimable amount
            (let
                (
                    (shares-owned (get shares-owned shareholder-position))
                    (per-share (get distribution-per-share distribution-data))
                    (claim-amount (* shares-owned per-share))
                )
                (begin
                    (asserts! (> claim-amount u0) err-insufficient-funds)
                    
                    ;; Transfer distribution to shareholder
                    (try! (as-contract (stx-transfer? claim-amount tx-sender tx-sender)))
                    
                    ;; Record claim
                    (map-set distribution-claims
                        { distribution-id: distribution-id, shareholder: tx-sender }
                        {
                            amount-claimed: claim-amount,
                            claim-date: u0
                        }
                    )
                    
                    ;; Update distribution total claimed
                    (map-set income-distributions
                        { distribution-id: distribution-id }
                        (merge distribution-data {
                            total-claimed: (+ (get total-claimed distribution-data) claim-amount)
                        })
                    )
                    
                    ;; Update shareholder position
                    (map-set shareholder-positions
                        { property-id: property-id, shareholder: tx-sender }
                        (merge shareholder-position {
                            total-received: (+ (get total-received shareholder-position) claim-amount),
                            last-claim-distribution: distribution-id
                        })
                    )
                    
                    ;; Update investor profile
                    (match (map-get? investor-profiles { investor: tx-sender })
                        profile (map-set investor-profiles
                            { investor: tx-sender }
                            (merge profile {
                                total-income-received: (+ (get total-income-received profile) claim-amount)
                            })
                        )
                        true
                    )
                    
                    (ok claim-amount)
                )
            )
        )
    )
)

;; Document Upload
(define-public (upload-property-document
    (property-id uint)
    (document-type (string-ascii 32))
    (document-hash (string-ascii 64))
    (document-url (string-ascii 128)))
    (begin
        (asserts! (not (var-get contract-paused)) err-contract-paused)
        (asserts! (is-property-owner property-id) err-unauthorized)
        (asserts! (> (len document-hash) u0) err-invalid-input)
        
        (map-set property-documents
            { property-id: property-id, document-type: document-type }
            {
                document-hash: document-hash,
                document-url: document-url,
                upload-date: u0,
                verified: false
            }
        )
        
        (ok true)
    )
)

;; Helper Functions (Optimized)
(define-private (update-investor-stats (investor principal) (shares uint) (amount uint))
    (match (map-get? investor-profiles { investor: investor })
        profile (begin
            (map-set investor-profiles
                { investor: investor }
                (merge profile {
                    properties-invested: (+ (get properties-invested profile) u1),
                    total-shares: (+ (get total-shares profile) shares),
                    total-invested: (+ (get total-invested profile) amount)
                })
            )
            true
        )
        true
    )
)

;; Optimized batch query functions
(define-public (get-multiple-properties (property-ids (list 20 uint)))
    (ok (map get-property-safe property-ids))
)

(define-private (get-property-safe (property-id uint))
    (map-get? rental-properties { property-id: property-id })
)

;; Read-Only Functions
(define-read-only (get-rental-property (property-id uint))
    (map-get? rental-properties { property-id: property-id })
)

(define-read-only (get-shareholder-position (property-id uint) (shareholder principal))
    (map-get? shareholder-positions { property-id: property-id, shareholder: shareholder })
)

(define-read-only (get-income-distribution (distribution-id uint))
    (map-get? income-distributions { distribution-id: distribution-id })
)

(define-read-only (get-distribution-claim (distribution-id uint) (shareholder principal))
    (map-get? distribution-claims { distribution-id: distribution-id, shareholder: shareholder })
)

(define-read-only (get-property-document (property-id uint) (document-type (string-ascii 32)))
    (map-get? property-documents { property-id: property-id, document-type: document-type })
)

(define-read-only (get-investor-profile (investor principal))
    (map-get? investor-profiles { investor: investor })
)

(define-read-only (get-property-stats (property-id uint))
    (map-get? property-stats { property-id: property-id })
)

(define-read-only (get-contract-info)
    {
        next-property-id: (var-get next-property-id),
        next-distribution-id: (var-get next-distribution-id),
        platform-fee: (var-get platform-fee),
        min-share-price: (var-get min-share-price),
        is-paused: (var-get contract-paused)
    }
)

(define-read-only (calculate-unclaimed-income (property-id uint) (shareholder principal))
    (match (map-get? shareholder-positions { property-id: property-id, shareholder: shareholder })
        position (let
            (
                (shares-owned (get shares-owned position))
                (last-claimed (get last-claim-distribution position))
                (total-unclaimed u0)
            )
            (some {
                shares-owned: shares-owned,
                estimated-unclaimed: total-unclaimed
            })
        )
        none
    )
)

;; Emergency Functions
(define-public (emergency-withdraw)
    (begin
        (asserts! (is-owner) err-owner-only)
        (asserts! (var-get contract-paused) err-unauthorized)
        (as-contract (stx-transfer? (stx-get-balance tx-sender) tx-sender contract-owner))
    )
)