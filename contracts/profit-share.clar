;; profit-share.clar
;; ------------------------------------------------------------
;; STX Profit Sharing Contract
;;
;; - Admin assigns shares to participants
;; - Anyone can deposit STX as profit
;; - Shareholders claim profits proportionally
;; - Uses cumulative profit-per-share accounting (no loops)
;; ------------------------------------------------------------

(define-constant ERR-NOT-ADMIN u100)
(define-constant ERR-NO-SHARES u101)
(define-constant ERR-NO-PROFIT u102)
(define-constant ERR-INVALID-AMOUNT u103)

;; Precision scaling (1e6)
(define-constant PRECISION u1000000)

;; -------------------------
;; Admin
;; -------------------------
(define-data-var admin principal tx-sender)

;; Update admin (only current admin can do this)
(define-public (set-admin (new-admin principal))
  (let ((sender tx-sender)
        (checked-admin (if (is-eq new-admin new-admin) new-admin tx-sender)))
    (asserts! (is-eq sender (var-get admin)) (err ERR-NOT-ADMIN))
    (var-set admin checked-admin)
    (ok true)
  )
)

;; -------------------------
;; Share Accounting
;; -------------------------

;; Total shares outstanding
(define-data-var total-shares uint u0)

;; Cumulative profit per share (scaled by 1e6)
(define-data-var cumulative-profit-per-share uint u0)

;; User share data
(define-map shareholders
  { user: principal }
  {
    shares: uint,
    reward-debt: uint   ;; tracks already-accounted profit
  })

;; -------------------------
;; Events
;; -------------------------

(define-private (ev-deposit (amount uint))
  (print { event: "profit-deposit", amount: amount }))

(define-private (ev-claim (user principal) (amount uint))
  (print { event: "profit-claimed", user: user, amount: amount }))

(define-private (ev-share-update (user principal) (shares uint))
  (print { event: "shares-updated", user: user, shares: shares }))

;; -------------------------
;; Admin Functions
;; -------------------------

;; Set or update shares for a user
(define-public (set-shares (user principal) (new-shares uint))
  (let ((checked-user (if (is-eq user user) user tx-sender))
        (admin-sender tx-sender))
    (asserts! (is-eq admin-sender (var-get admin)) (err ERR-NOT-ADMIN))

    (let (
          (existing (map-get? shareholders { user: checked-user }))
          (current-shares (if (is-some existing)
                              (get shares (unwrap-panic existing))
                              u0))
          (checked-shares (if (> new-shares u0) new-shares u0))
         )

      ;; update total shares
      (var-set total-shares
        (+ (- (var-get total-shares) current-shares) checked-shares))

      ;; update reward debt
      (let ((new-reward-debt (/ (* checked-shares (var-get cumulative-profit-per-share)) PRECISION)))
        (map-set shareholders
          { user: checked-user }
          {
            shares: checked-shares,
            reward-debt: new-reward-debt
          }))

      (ev-share-update checked-user checked-shares)
      (ok true)
    )
  )
)

;; -------------------------
;; Deposit Profit (STX)
;; -------------------------
(define-public (deposit-profit (amount uint))
  (begin
    (asserts! (> amount u0) (err ERR-INVALID-AMOUNT))
    (asserts! (> (var-get total-shares) u0) (err ERR-NO-SHARES))
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))

    ;; increase cumulative profit per share
    (var-set cumulative-profit-per-share
      (+ (var-get cumulative-profit-per-share)
         (/ (* amount PRECISION) (var-get total-shares))))

    (ev-deposit amount)
    (ok true)
  )
)

;; -------------------------
;; Claim Profit
;; -------------------------
(define-public (claim)
  (let ((user-data (map-get? shareholders { user: tx-sender })))
    (asserts! (is-some user-data) (err ERR-NO-SHARES))

    (let (
          (data (unwrap-panic user-data))
          (shares (get shares data))
          (reward-debt (get reward-debt data))
          (accumulated (/ (* shares (var-get cumulative-profit-per-share)) PRECISION))
          (pending (- accumulated reward-debt))
         )

      (asserts! (> pending u0) (err ERR-NO-PROFIT))

      ;; update reward debt
      (map-set shareholders
        { user: tx-sender }
        {
          shares: shares,
          reward-debt: accumulated
        })

      ;; transfer STX
      (try! (stx-transfer? pending (as-contract tx-sender) tx-sender))

      (ev-claim tx-sender pending)
      (ok pending)
    )
  )
)

;; -------------------------
;; Read-Only Functions
;; -------------------------

(define-read-only (get-total-shares)
  (ok (var-get total-shares)))

(define-read-only (get-user-info (user principal))
  (ok (map-get? shareholders { user: user })))

(define-read-only (pending-profit (user principal))
  (let ((user-data (map-get? shareholders { user: user })))
    (if (is-none user-data)
        (ok u0)
        (let (
              (data (unwrap-panic user-data))
              (shares (get shares data))
              (reward-debt (get reward-debt data))
              (accumulated (/ (* shares (var-get cumulative-profit-per-share)) PRECISION))
             )
          (ok (- accumulated reward-debt))
        )
    )
  )
)

(define-read-only (get-admin)
  (ok (var-get admin)))