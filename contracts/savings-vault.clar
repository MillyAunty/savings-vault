;; ------------------------------------------------------------
;; Savings Vault Smart Contract
;; Users deposit STX for a lock period to earn interest.
;; ------------------------------------------------------------

(define-constant ERR_ZERO_AMOUNT        (err u100))
(define-constant ERR_ALREADY_DEPOSITED  (err u101))
(define-constant ERR_NOTHING_TO_WITHDRAW (err u102))
(define-constant ERR_NOT_MATURED        (err u103))
(define-constant ERR_TRANSFER_FAILED    (err u104))

;; interest rate = 5% annual (approx)
(define-constant INTEREST_RATE u5)
(define-constant BLOCKS_PER_YEAR u52560) ;; ~10 min per block * 365 days

(define-data-var total-vault-balance uint u0)

;; store deposits
(define-map vault principal
  (tuple (amount uint)
         (start-block uint)
         (lock-period uint)
         (active bool)))

;; ------------------------------------------------------------
;; Deposit STX into the vault
;; ------------------------------------------------------------
(define-public (deposit (lock-period uint))
  (let ((amount (stx-get-balance tx-sender)))
    (begin
      (asserts! (> amount u0) ERR_ZERO_AMOUNT)
      (asserts! (> lock-period u0) ERR_ZERO_AMOUNT)
      (asserts! (is-none (map-get? vault tx-sender)) ERR_ALREADY_DEPOSITED)
      (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
      (map-set vault tx-sender
        (tuple (amount amount)
               (start-block burn-block-height)
               (lock-period lock-period)
               (active true)))
      (var-set total-vault-balance (+ (var-get total-vault-balance) amount))
      (ok (tuple (depositor tx-sender) (amount amount) (lock-period lock-period))))))

;; ------------------------------------------------------------
;; Calculate user's matured amount (principal + interest)
;; ------------------------------------------------------------
(define-read-only (calculate-matured (who principal))
  (match (map-get? vault who) record
    (let ((amount (get amount record))
          (start (get start-block record))
          (lock (get lock-period record))
          (end (+ start lock)))
      (if (>= burn-block-height end)
          (let ((interest (/ (* amount INTEREST_RATE lock) (* BLOCKS_PER_YEAR u100))))
            (ok (+ amount interest)))
          (err u1)))
    (err u2)))

;; ------------------------------------------------------------
;; Withdraw matured deposit
;; ------------------------------------------------------------
(define-public (withdraw)
  (match (map-get? vault tx-sender) record
    (let ((amount (get amount record))
          (start (get start-block record))
          (lock (get lock-period record))
          (active (get active record))
          (end (+ start lock)))
      (begin
        (asserts! active ERR_NOTHING_TO_WITHDRAW)
        (asserts! (>= burn-block-height end) ERR_NOT_MATURED)
        (let ((interest (/ (* amount INTEREST_RATE lock) (* BLOCKS_PER_YEAR u100)))
              (total (+ amount interest)))
          (match (stx-transfer? total (as-contract tx-sender) tx-sender)
            success (begin
                     (map-delete vault tx-sender)
                     (var-set total-vault-balance (- (var-get total-vault-balance) amount))
                     (ok (tuple (withdrawn total) (interest interest))))
            error ERR_TRANSFER_FAILED))))
    ERR_NOTHING_TO_WITHDRAW))

;; ------------------------------------------------------------
;; Read-only: view deposit info
;; ------------------------------------------------------------
(define-read-only (get-deposit (who principal))
  (ok (map-get? vault who))
)

(define-read-only (get-total-vault-balance)
  (ok (var-get total-vault-balance))
)