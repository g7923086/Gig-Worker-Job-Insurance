(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-not-active (err u104))
(define-constant err-already-claimed (err u105))
(define-constant err-not-eligible (err u106))
(define-constant err-invalid-amount (err u107))
(define-constant err-invalid-period (err u108))
(define-constant err-policy-expired (err u109))

(define-constant weekly-period u7)
(define-constant daily-period u1)
(define-constant max-claim-period u30)

(define-data-var fee-percentage uint u5)
(define-data-var min-premium-amount uint u1000000)
(define-data-var total-premiums uint u0)
(define-data-var total-claims uint u0)

(define-map policies
  { policy-id: uint }
  {
    owner: principal,
    premium-amount: uint,
    coverage-amount: uint,
    period-type: uint,
    start-block: uint,
    end-block: uint,
    active: bool,
    claimed: bool
  }
)

(define-map user-policies
  { user: principal }
  { policy-ids: (list 20 uint) }
)

(define-data-var next-policy-id uint u1)

(define-read-only (get-policy (policy-id uint))
  (map-get? policies { policy-id: policy-id })
)

(define-read-only (get-user-policies (user principal))
  (default-to { policy-ids: (list) } (map-get? user-policies { user: user }))
)

(define-read-only (get-fee-percentage)
  (var-get fee-percentage)
)

(define-read-only (get-min-premium)
  (var-get min-premium-amount)
)

(define-read-only (calculate-coverage (premium-amount uint) (period-type uint))
  (if (< premium-amount (var-get min-premium-amount))
    u0
    (if (is-eq period-type daily-period)
      (* premium-amount u10)
      (* premium-amount u8))
  )
)

(define-read-only (is-policy-active (policy-id uint))
  (match (map-get? policies { policy-id: policy-id })
    policy (and (get active policy) (< stacks-block-height (get end-block policy)))
    false
  )
)

(define-read-only (can-claim (policy-id uint))
  (match (map-get? policies { policy-id: policy-id })
    policy (and 
            (get active policy) 
            (not (get claimed policy))
            (>= stacks-block-height (+ (get start-block policy) u10)))
    false
  )
)

(define-public (set-fee-percentage (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee u20) (err u110))
    (ok (var-set fee-percentage new-fee))
  )
)

(define-public (set-min-premium (new-min uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok (var-set min-premium-amount new-min))
  )
)

(define-public (purchase-insurance (premium-amount uint) (period-type uint))
  (let (
    (policy-id (var-get next-policy-id))
    (coverage-amount (calculate-coverage premium-amount period-type))
    (period-blocks (if (is-eq period-type daily-period) u144 (* u144 u7)))
    (user-policy-list (get policy-ids (get-user-policies tx-sender)))
  )
    (asserts! (or (is-eq period-type daily-period) (is-eq period-type weekly-period)) err-invalid-period)
    (asserts! (>= premium-amount (var-get min-premium-amount)) err-invalid-amount)
    (asserts! (> coverage-amount u0) err-invalid-amount)
    (asserts! (is-ok (stx-transfer? premium-amount tx-sender (as-contract tx-sender))) err-insufficient-funds)
    
    (map-set policies
      { policy-id: policy-id }
      {
        owner: tx-sender,
        premium-amount: premium-amount,
        coverage-amount: coverage-amount,
        period-type: period-type,
        start-block: stacks-block-height,
        end-block: (+ stacks-block-height period-blocks),
        active: true,
        claimed: false
      }
    )
    
    (asserts! (< (len user-policy-list) u20) err-already-exists)
    (map-set user-policies
      { user: tx-sender }
      { policy-ids: (unwrap! (as-max-len? (append user-policy-list policy-id) u20) err-already-exists) }
    )
    
    (var-set next-policy-id (+ policy-id u1))
    (var-set total-premiums (+ (var-get total-premiums) premium-amount))
    
    (ok policy-id)
  )
)

(define-public (claim-insurance (policy-id uint))
  (let (
    (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
  )
    (asserts! (is-eq (get owner policy) tx-sender) err-owner-only)
    (asserts! (get active policy) err-not-active)
    (asserts! (not (get claimed policy)) err-already-claimed)
    (asserts! (< stacks-block-height (get end-block policy)) err-policy-expired)
    (asserts! (>= stacks-block-height (+ (get start-block policy) u10)) err-not-eligible)
    
    (map-set policies
      { policy-id: policy-id }
      (merge policy { claimed: true })
    )
    
    (var-set total-claims (+ (var-get total-claims) (get coverage-amount policy)))
    
    (as-contract (stx-transfer? (get coverage-amount policy) tx-sender (get owner policy)))
  )
)

(define-public (cancel-policy (policy-id uint))
  (let (
    (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
    (refund-amount (/ (get premium-amount policy) u2))
  )
    (asserts! (is-eq (get owner policy) tx-sender) err-owner-only)
    (asserts! (get active policy) err-not-active)
    (asserts! (not (get claimed policy)) err-already-claimed)
    
    (map-set policies
      { policy-id: policy-id }
      (merge policy { active: false })
    )
    
    (as-contract (stx-transfer? refund-amount tx-sender (get owner policy)))
  )
)

(define-public (withdraw-fees)
  (let (
    (balance (stx-get-balance (as-contract tx-sender)))
    (available (- balance (var-get total-premiums)))
  )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> available u0) err-insufficient-funds)
    
    (as-contract (stx-transfer? available tx-sender contract-owner))
  )
)


(define-constant discount-threshold-1 u2)
(define-constant discount-threshold-2 u4)
(define-constant discount-rate-1 u5)
(define-constant discount-rate-2 u10)

(define-read-only (get-user-active-policies-count (user principal))
  (let (
    (policy-list (get policy-ids (get-user-policies user)))
    (active-count (fold check-active-policy policy-list u0))
  )
    active-count
  )
)

(define-private (check-active-policy (policy-id uint) (count uint))
  (if (is-policy-active policy-id)
    (+ count u1)
    count
  )
)

(define-read-only (calculate-discounted-premium (premium-amount uint) (user principal))
  (let (
    (active-count (get-user-active-policies-count user))
    (discount-rate (if (>= active-count discount-threshold-2)
      discount-rate-2
      (if (>= active-count discount-threshold-1)
        discount-rate-1
        u0
      )
    ))
  )
    (- premium-amount (/ (* premium-amount discount-rate) u100))
  )
)


(define-public (get-discounted-premium (premium-amount uint) (user principal))
  (let (
    (discounted-premium (calculate-discounted-premium premium-amount user))
  )
    (ok discounted-premium)
  )
)
(define-public (get-coverage-amount (premium-amount uint) (period-type uint))
  (let (
    (coverage-amount (calculate-coverage premium-amount period-type))
  )
    (ok coverage-amount)
  )
)
(define-public (get-claimable-amount (policy-id uint))
  (let (
    (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
  )
    (asserts! (is-eq (get owner policy) tx-sender) err-owner-only)
    (asserts! (get active policy) err-not-active)
    (asserts! (not (get claimed policy)) err-already-claimed)
    (asserts! (< stacks-block-height (get end-block policy)) err-policy-expired)
    
    (ok (get coverage-amount policy))
  )
)(define-constant err-cannot-extend (err u111))
(define-constant extension-window u10)

(define-public (extend-policy (policy-id uint) (additional-premium uint))
  (let (
    (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
    (current-end-block (get end-block policy))
    (period-blocks (if (is-eq (get period-type policy) daily-period) u144 (* u144 u7)))
  )
    (asserts! (is-eq (get owner policy) tx-sender) err-owner-only)
    (asserts! (get active policy) err-not-active)
    (asserts! (not (get claimed policy)) err-already-claimed)
    (asserts! (>= (+ current-end-block extension-window) stacks-block-height) err-cannot-extend)
    (asserts! (>= additional-premium (var-get min-premium-amount)) err-invalid-amount)
    (asserts! (is-ok (stx-transfer? additional-premium tx-sender (as-contract tx-sender))) err-insufficient-funds)
    
    (map-set policies
      { policy-id: policy-id }
      (merge policy { 
        end-block: (+ current-end-block period-blocks),
        premium-amount: (+ (get premium-amount policy) additional-premium)
      })
    )
    
    (var-set total-premiums (+ (var-get total-premiums) additional-premium))
    (ok true)
  )
)