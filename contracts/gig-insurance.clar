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

(define-constant err-beneficiary-exists (err u112))
(define-constant err-beneficiary-not-found (err u113))
(define-constant err-max-beneficiaries (err u114))
(define-constant err-not-beneficiary (err u115))
(define-constant max-beneficiaries u5)
(define-constant beneficiary-claim-delay u144)

(define-map policy-beneficiaries
  { policy-id: uint }
  { beneficiaries: (list 5 principal) }
)

(define-map beneficiary-shares
  { policy-id: uint, beneficiary: principal }
  { share-percentage: uint }
)

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
  (begin
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
)

(define-constant err-cannot-extend (err u111))
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

(define-read-only (get-policy-beneficiaries (policy-id uint))
  (default-to { beneficiaries: (list) } (map-get? policy-beneficiaries { policy-id: policy-id }))
)

(define-read-only (get-beneficiary-share (policy-id uint) (beneficiary principal))
  (default-to { share-percentage: u0 } (map-get? beneficiary-shares { policy-id: policy-id, beneficiary: beneficiary }))
)

(define-read-only (calculate-beneficiary-payout (policy-id uint) (beneficiary principal))
  (match (map-get? policies { policy-id: policy-id })
    policy (let (
      (coverage (get coverage-amount policy))
      (share (get share-percentage (get-beneficiary-share policy-id beneficiary)))
    )
      (/ (* coverage share) u100)
    )
    u0
  )
)

(define-public (add-beneficiary (policy-id uint) (beneficiary principal) (share-percentage uint))
  (let (
    (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
    (current-beneficiaries (get beneficiaries (get-policy-beneficiaries policy-id)))
  )
    (asserts! (is-eq (get owner policy) tx-sender) err-owner-only)
    (asserts! (get active policy) err-not-active)
    (asserts! (not (get claimed policy)) err-already-claimed)
    (asserts! (< (len current-beneficiaries) max-beneficiaries) err-max-beneficiaries)
    (asserts! (is-none (index-of current-beneficiaries beneficiary)) err-beneficiary-exists)
    (asserts! (and (> share-percentage u0) (<= share-percentage u100)) err-invalid-amount)
    
    (map-set policy-beneficiaries
      { policy-id: policy-id }
      { beneficiaries: (unwrap! (as-max-len? (append current-beneficiaries beneficiary) u5) err-max-beneficiaries) }
    )
    
    (map-set beneficiary-shares
      { policy-id: policy-id, beneficiary: beneficiary }
      { share-percentage: share-percentage }
    )
    
    (ok true)
  )
)



(define-public (beneficiary-claim (policy-id uint))
  (let (
    (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
    (beneficiaries (get beneficiaries (get-policy-beneficiaries policy-id)))
    (payout-amount (calculate-beneficiary-payout policy-id tx-sender))
  )
    (asserts! (is-some (index-of beneficiaries tx-sender)) err-not-beneficiary)
    (asserts! (get active policy) err-not-active)
    (asserts! (not (get claimed policy)) err-already-claimed)
    (asserts! (< stacks-block-height (get end-block policy)) err-policy-expired)
    (asserts! (>= stacks-block-height (+ (get start-block policy) beneficiary-claim-delay)) err-not-eligible)
    (asserts! (> payout-amount u0) err-invalid-amount)
    
    (map-set policies
      { policy-id: policy-id }
      (merge policy { claimed: true })
    )
    
    (var-set total-claims (+ (var-get total-claims) payout-amount))
    
    (as-contract (stx-transfer? payout-amount tx-sender tx-sender))
  )
)

(define-public (update-beneficiary-share (policy-id uint) (beneficiary principal) (new-share uint))
  (let (
    (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
    (beneficiaries (get beneficiaries (get-policy-beneficiaries policy-id)))
  )
    (asserts! (is-eq (get owner policy) tx-sender) err-owner-only)
    (asserts! (get active policy) err-not-active)
    (asserts! (not (get claimed policy)) err-already-claimed)
    (asserts! (is-some (index-of beneficiaries beneficiary)) err-beneficiary-not-found)
    (asserts! (and (> new-share u0) (<= new-share u100)) err-invalid-amount)
    
    (map-set beneficiary-shares
      { policy-id: policy-id, beneficiary: beneficiary }
      { share-percentage: new-share }
    )
    
    (ok true)
  )
)

(define-constant err-invalid-risk-score (err u116))
(define-constant err-risk-assessment-failed (err u117))
(define-constant err-insufficient-history (err u118))
(define-constant err-invalid-market-factor (err u119))
(define-constant err-risk-tier-not-found (err u120))

(define-constant max-risk-score u1000)
(define-constant min-risk-score u0)
(define-constant base-risk-score u500)
(define-constant history-requirement u3)
(define-constant claim-penalty u100)
(define-constant loyalty-bonus u50)
(define-constant max-market-adjustment u200)

(define-data-var market-volatility-factor uint u100)
(define-data-var global-claim-rate uint u10)
(define-data-var assessment-window uint u1008)

(define-map user-risk-profiles
  { user: principal }
  {
    risk-score: uint,
    total-policies: uint,
    total-claims: uint,
    last-claim-block: uint,
    assessment-date: uint,
    tier: uint
  }
)

(define-map risk-tiers
  { tier: uint }
  {
    min-score: uint,
    max-score: uint,
    premium-multiplier: uint,
    max-coverage-multiplier: uint,
    name: (string-ascii 20)
  }
)

(define-map market-conditions
  { period: uint }
  {
    volatility: uint,
    claim-frequency: uint,
    adjustment-factor: uint,
    timestamp: uint
  }
)

(define-private (initialize-risk-tiers)
  (begin
    (map-set risk-tiers { tier: u1 } { min-score: u0, max-score: u200, premium-multiplier: u150, max-coverage-multiplier: u80, name: "high-risk" })
    (map-set risk-tiers { tier: u2 } { min-score: u201, max-score: u400, premium-multiplier: u120, max-coverage-multiplier: u90, name: "medium-high-risk" })
    (map-set risk-tiers { tier: u3 } { min-score: u401, max-score: u600, premium-multiplier: u100, max-coverage-multiplier: u100, name: "standard-risk" })
    (map-set risk-tiers { tier: u4 } { min-score: u601, max-score: u800, premium-multiplier: u85, max-coverage-multiplier: u110, name: "low-risk" })
    (map-set risk-tiers { tier: u5 } { min-score: u801, max-score: u1000, premium-multiplier: u70, max-coverage-multiplier: u120, name: "premium-low-risk" })
    (ok true)
  )
)

(define-read-only (get-user-risk-profile (user principal))
  (match (map-get? user-risk-profiles { user: user })
    profile profile
    {
      risk-score: base-risk-score,
      total-policies: u0,
      total-claims: u0,
      last-claim-block: u0,
      assessment-date: u0,
      tier: u3
    }
  )
)

(define-read-only (get-risk-tier (tier uint))
  (map-get? risk-tiers { tier: tier })
)

(define-read-only (get-market-conditions (period uint))
  (map-get? market-conditions { period: period })
)

(define-read-only (calculate-risk-score (user principal))
  (let (
    (profile (get-user-risk-profile user))
    (base-score (get risk-score profile))
    (policy-count (get total-policies profile))
    (claim-count (get total-claims profile))
    (last-claim-block (get last-claim-block profile))
    (blocks-since-claim (- stacks-block-height last-claim-block))
    
    (claim-ratio (if (> policy-count u0) (/ (* claim-count u100) policy-count) u0))
    (claim-penalty-score (if (> claim-ratio u20) (* claim-penalty (/ claim-ratio u20)) u0))
    (loyalty-bonus-score (if (> policy-count u5) (* loyalty-bonus (/ policy-count u5)) u0))
    (time-bonus (if (> blocks-since-claim u4320) u25 u0))
    
    (adjusted-score (+ base-score loyalty-bonus-score time-bonus))
    (final-score (if (> adjusted-score claim-penalty-score) (- adjusted-score claim-penalty-score) u0))
  )
    (if (> final-score max-risk-score) max-risk-score final-score)
  )
)

(define-read-only (determine-risk-tier (risk-score uint))
  (if (and (>= risk-score u0) (<= risk-score u200)) u1
    (if (and (>= risk-score u201) (<= risk-score u400)) u2
      (if (and (>= risk-score u401) (<= risk-score u600)) u3
        (if (and (>= risk-score u601) (<= risk-score u800)) u4
          (if (and (>= risk-score u801) (<= risk-score u1000)) u5
            u3
          )
        )
      )
    )
  )
)

(define-read-only (calculate-risk-adjusted-premium (base-premium uint) (user principal))
  (let (
    (risk-score (calculate-risk-score user))
    (tier (determine-risk-tier risk-score))
    (tier-info (default-to { premium-multiplier: u100, max-coverage-multiplier: u100, min-score: u0, max-score: u0, name: "standard" } (get-risk-tier tier)))
    (premium-multiplier (get premium-multiplier tier-info))
    (market-factor (var-get market-volatility-factor))
    (global-claim-factor (var-get global-claim-rate))
    
    (market-adjusted-multiplier (+ premium-multiplier (if (> market-factor u100) (- market-factor u100) u0)))
    (claim-adjusted-multiplier (+ market-adjusted-multiplier (if (> global-claim-factor u15) (* (- global-claim-factor u15) u5) u0)))
    
    (adjusted-premium (/ (* base-premium claim-adjusted-multiplier) u100))
  )
    adjusted-premium
  )
)

(define-read-only (calculate-risk-adjusted-coverage (base-coverage uint) (user principal))
  (let (
    (risk-score (calculate-risk-score user))
    (tier (determine-risk-tier risk-score))
    (tier-info (default-to { premium-multiplier: u100, max-coverage-multiplier: u100, min-score: u0, max-score: u0, name: "standard" } (get-risk-tier tier)))
    (coverage-multiplier (get max-coverage-multiplier tier-info))
    
    (adjusted-coverage (/ (* base-coverage coverage-multiplier) u100))
  )
    adjusted-coverage
  )
)

(define-public (update-risk-assessment (user principal))
  (let (
    (current-profile (get-user-risk-profile user))
    (new-risk-score (calculate-risk-score user))
    (new-tier (determine-risk-tier new-risk-score))
  )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-risk-score max-risk-score) err-invalid-risk-score)
    (asserts! (>= new-risk-score min-risk-score) err-invalid-risk-score)
    
    (map-set user-risk-profiles
      { user: user }
      (merge current-profile {
        risk-score: new-risk-score,
        assessment-date: stacks-block-height,
        tier: new-tier
      })
    )
    
    (ok new-risk-score)
  )
)

(define-public (update-market-conditions (volatility uint) (claim-frequency uint) (adjustment-factor uint))
  (let (
    (current-period (/ stacks-block-height u1008))
  )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= volatility u300) err-invalid-market-factor)
    (asserts! (<= claim-frequency u100) err-invalid-market-factor)
    (asserts! (<= adjustment-factor max-market-adjustment) err-invalid-market-factor)
    
    (map-set market-conditions
      { period: current-period }
      {
        volatility: volatility,
        claim-frequency: claim-frequency,
        adjustment-factor: adjustment-factor,
        timestamp: stacks-block-height
      }
    )
    
    (var-set market-volatility-factor volatility)
    (var-set global-claim-rate claim-frequency)
    
    (ok true)
  )
)

(define-public (update-user-policy-stats (user principal) (policy-count uint) (claim-count uint) (last-claim-block uint))
  (let (
    (current-profile (get-user-risk-profile user))
  )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    (map-set user-risk-profiles
      { user: user }
      (merge current-profile {
        total-policies: policy-count,
        total-claims: claim-count,
        last-claim-block: last-claim-block
      })
    )
    
    (ok true)
  )
)

(define-public (purchase-risk-assessed-insurance (base-premium uint) (period-type uint))
  (let (
    (risk-adjusted-premium (calculate-risk-adjusted-premium base-premium tx-sender))
    (base-coverage (calculate-coverage risk-adjusted-premium period-type))
    (risk-adjusted-coverage (calculate-risk-adjusted-coverage base-coverage tx-sender))
    (policy-id (var-get next-policy-id))
    (period-blocks (if (is-eq period-type daily-period) u144 (* u144 u7)))
    (user-policy-list (get policy-ids (get-user-policies tx-sender)))
    (current-profile (get-user-risk-profile tx-sender))
  )
    (asserts! (or (is-eq period-type daily-period) (is-eq period-type weekly-period)) err-invalid-period)
    (asserts! (>= risk-adjusted-premium (var-get min-premium-amount)) err-invalid-amount)
    (asserts! (> risk-adjusted-coverage u0) err-invalid-amount)
    (asserts! (is-ok (stx-transfer? risk-adjusted-premium tx-sender (as-contract tx-sender))) err-insufficient-funds)
    
    (map-set policies
      { policy-id: policy-id }
      {
        owner: tx-sender,
        premium-amount: risk-adjusted-premium,
        coverage-amount: risk-adjusted-coverage,
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
    
    (map-set user-risk-profiles
      { user: tx-sender }
      (merge current-profile {
        total-policies: (+ (get total-policies current-profile) u1)
      })
    )
    
    (var-set next-policy-id (+ policy-id u1))
    (var-set total-premiums (+ (var-get total-premiums) risk-adjusted-premium))
    
    (ok policy-id)
  )
)

(define-public (get-risk-assessment-quote (base-premium uint) (period-type uint) (user principal))
  (let (
    (risk-adjusted-premium (calculate-risk-adjusted-premium base-premium user))
    (base-coverage (calculate-coverage risk-adjusted-premium period-type))
    (risk-adjusted-coverage (calculate-risk-adjusted-coverage base-coverage user))
    (risk-score (calculate-risk-score user))
    (tier (determine-risk-tier risk-score))
  )
    (ok {
      adjusted-premium: risk-adjusted-premium,
      adjusted-coverage: risk-adjusted-coverage,
      risk-score: risk-score,
      tier: tier,
      base-premium: base-premium,
      base-coverage: base-coverage
    })
  )
)

(define-public (initialize-system)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (initialize-risk-tiers)
  )
)

;; Emergency Claims Processing System
;; Provides immediate partial payouts for verified emergencies

(define-constant err-emergency-not-found (err u121))
(define-constant err-emergency-cooldown (err u122))
(define-constant err-invalid-emergency-type (err u123))
(define-constant err-emergency-contact-required (err u124))
(define-constant err-insufficient-emergency-reserves (err u125))
(define-constant err-emergency-already-processed (err u126))
(define-constant err-emergency-verification-failed (err u127))

;; Appeal system error codes
(define-constant err-not-appealable (err u128))
(define-constant err-appeal-expired (err u129))
(define-constant err-appeal-exists (err u130))
(define-constant err-appeal-not-found (err u131))
(define-constant err-already-voted (err u132))
(define-constant err-appeal-not-ready (err u133))
(define-constant err-insufficient-voting-power (err u134))

;; Appeal system constants
(define-constant appeal-window-blocks u1008) ;; 7 days
(define-constant min-voting-power u1000000) ;; Minimum premium to vote
(define-constant appeal-quorum u3) ;; Minimum number of votes needed

;; Appeal statuses
(define-constant appeal-status-pending u0)
(define-constant appeal-status-approved u1)
(define-constant appeal-status-rejected u2)
(define-constant appeal-status-expired u3)

;; Claim dispute states
(define-map claim-disputes
  { policy-id: uint }
  {
    claimant: principal,
    dispute-reason: (string-ascii 200),
    dispute-block: uint,
    status: uint, ;; 0=pending, 1=under-review, 2=denied, 3=approved
    reviewed-by: principal,
    review-block: uint
  }
)

;; Appeals tracking
(define-map appeals
  { appeal-id: uint }
  {
    policy-id: uint,
    appellant: principal,
    appeal-reason: (string-ascii 200),
    submission-block: uint,
    deadline-block: uint,
    votes-for: uint,
    votes-against: uint,
    total-voting-power-for: uint,
    total-voting-power-against: uint,
    status: uint,
    resolved-block: uint
  }
)

;; Appeal votes tracking
(define-map appeal-votes
  { appeal-id: uint, voter: principal }
  {
    vote: bool, ;; true = for, false = against
    voting-power: uint,
    vote-block: uint
  }
)

(define-data-var next-appeal-id uint u1)

;; Emergency types
(define-constant emergency-medical u1)
(define-constant emergency-accident u2)
(define-constant emergency-equipment-failure u3)
(define-constant emergency-hospitalization u4)
(define-constant emergency-vehicle-breakdown u5)

;; Emergency system parameters
(define-constant emergency-cooldown-blocks u720)
(define-constant max-emergency-claims-per-policy u3)
(define-constant emergency-verification-window u144)

(define-data-var emergency-reserves-percentage uint u15)
(define-data-var total-emergency-payouts uint u0)

;; Emergency type configurations
(define-map emergency-type-configs
  { emergency-type: uint }
  {
    payout-percentage: uint,
    min-verification-delay: uint,
    requires-contact: bool,
    max-amount-per-claim: uint,
    active: bool
  }
)

;; User emergency contacts
(define-map emergency-contacts
  { user: principal }
  {
    primary-contact: principal,
    secondary-contact: principal,
    last-updated: uint
  }
)

;; Emergency claims tracking
(define-map emergency-claims
  { claim-id: uint }
  {
    policy-id: uint,
    claimant: principal,
    emergency-type: uint,
    requested-amount: uint,
    approved-amount: uint,
    verification-contact: principal,
    claim-block: uint,
    verification-block: uint,
    status: uint,
    verification-data: (string-ascii 100)
  }
)

;; User emergency history
(define-map user-emergency-history
  { user: principal }
  {
    total-emergency-claims: uint,
    last-emergency-claim-block: uint,
    total-emergency-payouts: uint
  }
)

(define-data-var next-emergency-claim-id uint u1)

;; Initialize emergency type configurations
(define-private (initialize-emergency-types)
  (begin
    (map-set emergency-type-configs { emergency-type: emergency-medical }
      { payout-percentage: u50, min-verification-delay: u72, requires-contact: true, max-amount-per-claim: u5000000, active: true })
    (map-set emergency-type-configs { emergency-type: emergency-accident }
      { payout-percentage: u60, min-verification-delay: u48, requires-contact: true, max-amount-per-claim: u7000000, active: true })
    (map-set emergency-type-configs { emergency-type: emergency-equipment-failure }
      { payout-percentage: u30, min-verification-delay: u24, requires-contact: false, max-amount-per-claim: u2000000, active: true })
    (map-set emergency-type-configs { emergency-type: emergency-hospitalization }
      { payout-percentage: u70, min-verification-delay: u96, requires-contact: true, max-amount-per-claim: u10000000, active: true })
    (map-set emergency-type-configs { emergency-type: emergency-vehicle-breakdown }
      { payout-percentage: u40, min-verification-delay: u12, requires-contact: false, max-amount-per-claim: u3000000, active: true })
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-emergency-type-config (emergency-type uint))
  (map-get? emergency-type-configs { emergency-type: emergency-type })
)

(define-read-only (get-emergency-contacts (user principal))
  (map-get? emergency-contacts { user: user })
)

(define-read-only (get-emergency-claim (claim-id uint))
  (map-get? emergency-claims { claim-id: claim-id })
)

(define-read-only (get-user-emergency-history (user principal))
  (default-to 
    { total-emergency-claims: u0, last-emergency-claim-block: u0, total-emergency-payouts: u0 }
    (map-get? user-emergency-history { user: user })
  )
)

(define-read-only (calculate-emergency-payout (policy-id uint) (emergency-type uint))
  (match (map-get? policies { policy-id: policy-id })
    policy 
    (match (get-emergency-type-config emergency-type)
      config 
      (let (
        (coverage (get coverage-amount policy))
        (payout-percentage (get payout-percentage config))
        (max-amount (get max-amount-per-claim config))
        (calculated-amount (/ (* coverage payout-percentage) u100))
      )
        (if (> calculated-amount max-amount) max-amount calculated-amount)
      )
      u0
    )
    u0
  )
)

(define-read-only (get-available-emergency-reserves)
  (let (
    (total-premium-pool (var-get total-premiums))
    (reserves-percentage (var-get emergency-reserves-percentage))
    (total-payouts (var-get total-emergency-payouts))
    (available-reserves (/ (* total-premium-pool reserves-percentage) u100))
  )
    (if (> available-reserves total-payouts) (- available-reserves total-payouts) u0)
  )
)

;; Set emergency contacts
(define-public (set-emergency-contacts (primary principal) (secondary principal))
  (begin
    (asserts! (not (is-eq primary secondary)) err-invalid-amount)
    (asserts! (not (is-eq primary tx-sender)) err-invalid-amount)
    (asserts! (not (is-eq secondary tx-sender)) err-invalid-amount)
    
    (map-set emergency-contacts
      { user: tx-sender }
      {
        primary-contact: primary,
        secondary-contact: secondary,
        last-updated: stacks-block-height
      }
    )
    (ok true)
  )
)

;; Submit emergency claim
(define-public (submit-emergency-claim (policy-id uint) (emergency-type uint) (verification-data (string-ascii 100)))
  (let (
    (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
    (emergency-config (unwrap! (get-emergency-type-config emergency-type) err-invalid-emergency-type))
    (user-history (get-user-emergency-history tx-sender))
    (claim-id (var-get next-emergency-claim-id))
    (payout-amount (calculate-emergency-payout policy-id emergency-type))
    (available-reserves (get-available-emergency-reserves))
  )
    (asserts! (is-eq (get owner policy) tx-sender) err-owner-only)
    (asserts! (get active policy) err-not-active)
    (asserts! (not (get claimed policy)) err-already-claimed)
    (asserts! (get active emergency-config) err-invalid-emergency-type)
    (asserts! (< (get total-emergency-claims user-history) max-emergency-claims-per-policy) err-emergency-already-processed)
    (asserts! (>= (- stacks-block-height (get last-emergency-claim-block user-history)) emergency-cooldown-blocks) err-emergency-cooldown)
    (asserts! (>= available-reserves payout-amount) err-insufficient-emergency-reserves)
    
    (if (get requires-contact emergency-config)
      (asserts! (is-some (get-emergency-contacts tx-sender)) err-emergency-contact-required)
      true
    )
    
    (map-set emergency-claims
      { claim-id: claim-id }
      {
        policy-id: policy-id,
        claimant: tx-sender,
        emergency-type: emergency-type,
        requested-amount: payout-amount,
        approved-amount: u0,
        verification-contact: (match (get-emergency-contacts tx-sender) 
                                contacts (get primary-contact contacts)
                                tx-sender),
        claim-block: stacks-block-height,
        verification-block: u0,
        status: u0,
        verification-data: verification-data
      }
    )
    
    (map-set user-emergency-history
      { user: tx-sender }
      {
        total-emergency-claims: (+ (get total-emergency-claims user-history) u1),
        last-emergency-claim-block: stacks-block-height,
        total-emergency-payouts: (get total-emergency-payouts user-history)
      }
    )
    
    (var-set next-emergency-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

;; Verify and approve emergency claim
(define-public (verify-emergency-claim (claim-id uint))
  (let (
    (claim (unwrap! (get-emergency-claim claim-id) err-emergency-not-found))
    (emergency-config (unwrap! (get-emergency-type-config (get emergency-type claim)) err-invalid-emergency-type))
    (user-contacts (get-emergency-contacts (get claimant claim)))
    (min-delay (get min-verification-delay emergency-config))
    (payout-amount (get requested-amount claim))
    (user-history (get-user-emergency-history (get claimant claim)))
  )
    (asserts! (is-eq (get status claim) u0) err-emergency-already-processed)
    (asserts! (>= (- stacks-block-height (get claim-block claim)) min-delay) err-emergency-verification-failed)
    (asserts! (< (- stacks-block-height (get claim-block claim)) emergency-verification-window) err-emergency-verification-failed)
    
    (if (get requires-contact emergency-config)
      (match user-contacts
        contacts (asserts! (or (is-eq tx-sender (get primary-contact contacts)) 
                             (is-eq tx-sender (get secondary-contact contacts))
                             (is-eq tx-sender contract-owner)) err-emergency-contact-required)
        (asserts! (is-eq tx-sender contract-owner) err-emergency-contact-required)
      )
      true
    )
    
    (map-set emergency-claims
      { claim-id: claim-id }
      (merge claim {
        approved-amount: payout-amount,
        verification-block: stacks-block-height,
        status: u2
      })
    )
    
    (map-set user-emergency-history
      { user: (get claimant claim) }
      (merge user-history {
        total-emergency-payouts: (+ (get total-emergency-payouts user-history) payout-amount)
      })
    )
    
    (var-set total-emergency-payouts (+ (var-get total-emergency-payouts) payout-amount))
    
    (as-contract (stx-transfer? payout-amount tx-sender (get claimant claim)))
  )
)

;; Configure emergency type (admin only)
(define-public (configure-emergency-type (emergency-type uint) (payout-percentage uint) (min-delay uint) (requires-contact bool) (max-amount uint) (active bool))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= payout-percentage u100) err-invalid-amount)
    (asserts! (> max-amount u0) err-invalid-amount)
    
    (map-set emergency-type-configs
      { emergency-type: emergency-type }
      {
        payout-percentage: payout-percentage,
        min-verification-delay: min-delay,
        requires-contact: requires-contact,
        max-amount-per-claim: max-amount,
        active: active
      }
    )
    (ok true)
  )
)

;; Set emergency reserves percentage (admin only)
(define-public (set-emergency-reserves-percentage (new-percentage uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-percentage u50) err-invalid-amount)
    (var-set emergency-reserves-percentage new-percentage)
    (ok true)
  )
)

;; Initialize emergency system
(define-public (initialize-emergency-system)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (initialize-emergency-types)
  )
)

;; === CLAIM DISPUTE AND APPEAL SYSTEM ===

;; Submit a claim dispute (admin function)
(define-public (dispute-claim (policy-id uint) (dispute-reason (string-ascii 200)))
  (let (
    (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
  )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (get active policy) err-not-active)
    (asserts! (not (get claimed policy)) err-already-claimed)
    (asserts! (is-none (map-get? claim-disputes { policy-id: policy-id })) err-already-exists)
    
    (map-set claim-disputes
      { policy-id: policy-id }
      {
        claimant: (get owner policy),
        dispute-reason: dispute-reason,
        dispute-block: stacks-block-height,
        status: u2, ;; denied
        reviewed-by: tx-sender,
        review-block: stacks-block-height
      }
    )
    (ok true)
  )
)

;; Submit an appeal on a disputed claim
(define-public (submit-appeal (policy-id uint) (appeal-reason (string-ascii 200)))
  (let (
    (policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
    (dispute (unwrap! (map-get? claim-disputes { policy-id: policy-id }) err-not-found))
    (appeal-id (var-get next-appeal-id))
    (deadline (+ stacks-block-height appeal-window-blocks))
  )
    (asserts! (is-eq (get owner policy) tx-sender) err-owner-only)
    (asserts! (is-eq (get claimant dispute) tx-sender) err-owner-only)
    (asserts! (is-eq (get status dispute) u2) err-not-appealable) ;; only denied claims can be appealed
    (asserts! (<= (- stacks-block-height (get dispute-block dispute)) appeal-window-blocks) err-appeal-expired)
    
    (map-set appeals
      { appeal-id: appeal-id }
      {
        policy-id: policy-id,
        appellant: tx-sender,
        appeal-reason: appeal-reason,
        submission-block: stacks-block-height,
        deadline-block: deadline,
        votes-for: u0,
        votes-against: u0,
        total-voting-power-for: u0,
        total-voting-power-against: u0,
        status: appeal-status-pending,
        resolved-block: u0
      }
    )
    
    (var-set next-appeal-id (+ appeal-id u1))
    (ok appeal-id)
  )
)

;; Calculate voting power based on user's total premium contributions
(define-private (calculate-voting-power (voter principal))
  (let (
    (user-policy-list (get policy-ids (get-user-policies voter)))
    (total-premium (fold sum-user-premiums user-policy-list u0))
  )
    (if (>= total-premium min-voting-power) total-premium u0)
  )
)

;; Helper function to sum premiums
(define-private (sum-user-premiums (policy-id uint) (total uint))
  (match (map-get? policies { policy-id: policy-id })
    policy (+ total (get premium-amount policy))
    total
  )
)

;; Vote on an appeal
(define-public (vote-on-appeal (appeal-id uint) (vote-for bool))
  (let (
    (appeal (unwrap! (map-get? appeals { appeal-id: appeal-id }) err-appeal-not-found))
    (voting-power (calculate-voting-power tx-sender))
  )
    (asserts! (> voting-power u0) err-insufficient-voting-power)
    (asserts! (is-eq (get status appeal) appeal-status-pending) err-appeal-not-ready)
    (asserts! (< stacks-block-height (get deadline-block appeal)) err-appeal-expired)
    (asserts! (is-none (map-get? appeal-votes { appeal-id: appeal-id, voter: tx-sender })) err-already-voted)
    
    ;; Record the vote
    (map-set appeal-votes
      { appeal-id: appeal-id, voter: tx-sender }
      {
        vote: vote-for,
        voting-power: voting-power,
        vote-block: stacks-block-height
      }
    )
    
    ;; Update appeal vote counts
    (map-set appeals
      { appeal-id: appeal-id }
      (merge appeal {
        votes-for: (if vote-for (+ (get votes-for appeal) u1) (get votes-for appeal)),
        votes-against: (if vote-for (get votes-against appeal) (+ (get votes-against appeal) u1)),
        total-voting-power-for: (if vote-for 
                                  (+ (get total-voting-power-for appeal) voting-power) 
                                  (get total-voting-power-for appeal)),
        total-voting-power-against: (if vote-for 
                                      (get total-voting-power-against appeal)
                                      (+ (get total-voting-power-against appeal) voting-power))
      })
    )
    
    (ok true)
  )
)

;; Process appeal after voting deadline
(define-public (process-appeal (appeal-id uint))
  (let (
    (appeal (unwrap! (map-get? appeals { appeal-id: appeal-id }) err-appeal-not-found))
    (total-votes (+ (get votes-for appeal) (get votes-against appeal)))
    (votes-for (get votes-for appeal))
    (voting-power-for (get total-voting-power-for appeal))
    (voting-power-against (get total-voting-power-against appeal))
    (total-voting-power (+ voting-power-for voting-power-against))
  )
    (asserts! (is-eq (get status appeal) appeal-status-pending) err-appeal-not-ready)
    (asserts! (>= stacks-block-height (get deadline-block appeal)) err-appeal-not-ready)
    
    (let (
      (appeal-approved (and 
                        (>= total-votes appeal-quorum)
                        (> voting-power-for voting-power-against)
                        (>= (/ (* voting-power-for u100) total-voting-power) u51)))
      (new-status (if appeal-approved appeal-status-approved appeal-status-rejected))
    )
      (map-set appeals
        { appeal-id: appeal-id }
        (merge appeal {
          status: new-status,
          resolved-block: stacks-block-height
        })
      )
      
      ;; If appeal is approved, update the dispute status
      (if appeal-approved
        (match (map-get? claim-disputes { policy-id: (get policy-id appeal) })
          dispute (map-set claim-disputes
                    { policy-id: (get policy-id appeal) }
                    (merge dispute { status: u3 })) ;; approved
          true
        )
        true
      )
      
      (ok appeal-approved)
    )
  )
)

;; Helper function to list appeals (for checking existence)
(define-private (list-appeals (appeal-id uint))
  appeal-id
)

;; === READ-ONLY APPEAL QUERY FUNCTIONS ===

;; Get claim dispute details
(define-read-only (get-claim-dispute (policy-id uint))
  (map-get? claim-disputes { policy-id: policy-id })
)

;; Get appeal details
(define-read-only (get-appeal (appeal-id uint))
  (map-get? appeals { appeal-id: appeal-id })
)

;; Get user's vote on an appeal
(define-read-only (get-appeal-vote (appeal-id uint) (voter principal))
  (map-get? appeal-votes { appeal-id: appeal-id, voter: voter })
)

;; Check if a claim can be appealed
(define-read-only (can-appeal-claim (policy-id uint))
  (match (map-get? claim-disputes { policy-id: policy-id })
    dispute (and 
              (is-eq (get status dispute) u2) ;; denied
              (<= (- stacks-block-height (get dispute-block dispute)) appeal-window-blocks))
    false
  )
)

;; Check if user can vote on appeal
(define-read-only (can-vote-on-appeal (appeal-id uint) (voter principal))
  (match (map-get? appeals { appeal-id: appeal-id })
    appeal (let (
             (voting-power (calculate-voting-power voter))
           )
           (and 
             (> voting-power u0)
             (is-eq (get status appeal) appeal-status-pending)
             (< stacks-block-height (get deadline-block appeal))
             (is-none (map-get? appeal-votes { appeal-id: appeal-id, voter: voter }))))
    false
  )
)

;; Get appeal voting summary
(define-read-only (get-appeal-voting-summary (appeal-id uint))
  (match (map-get? appeals { appeal-id: appeal-id })
    appeal (some {
      appeal-id: appeal-id,
      policy-id: (get policy-id appeal),
      status: (get status appeal),
      votes-for: (get votes-for appeal),
      votes-against: (get votes-against appeal),
      total-voting-power-for: (get total-voting-power-for appeal),
      total-voting-power-against: (get total-voting-power-against appeal),
      deadline-block: (get deadline-block appeal),
      resolved-block: (get resolved-block appeal)
    })
    none
  )
)

;; Get user's voting power
(define-read-only (get-user-voting-power (user principal))
  (calculate-voting-power user)
)

;; Check if appeal is ready to be processed
(define-read-only (can-process-appeal (appeal-id uint))
  (match (map-get? appeals { appeal-id: appeal-id })
    appeal (and 
             (is-eq (get status appeal) appeal-status-pending)
             (>= stacks-block-height (get deadline-block appeal)))
    false
  )
)

;; Check if there's an existing appeal for a policy
(define-read-only (has-active-appeal (policy-id uint))
  ;; This is a simple check - in a production system you might want to iterate through appeals
  ;; For now, we'll implement a basic version
  (is-some (map-get? claim-disputes { policy-id: policy-id }))
)



