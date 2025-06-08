(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-STREAM-NOT-FOUND (err u101))
(define-constant ERR-INSUFFICIENT-BALANCE (err u102))
(define-constant ERR-INVALID-TIME (err u103))
(define-constant ERR-STREAM-ENDED (err u104))

(define-data-var contract-owner principal tx-sender)

(define-map salary-streams
    { stream-id: uint }
    {
        employer: principal,
        employee: principal,
        total-amount: uint,
        start-time: uint,
        end-time: uint,
        amount-paid: uint,
        is-active: bool,
    }
)

(define-map employer-balances
    { employer: principal }
    { balance: uint }
)

(define-data-var stream-nonce uint u0)

(define-read-only (get-stream (stream-id uint))
    (map-get? salary-streams { stream-id: stream-id })
)

(define-read-only (get-employer-balance (employer principal))
    (default-to { balance: u0 }
        (map-get? employer-balances { employer: employer })
    )
)

(define-read-only (calculate-streamed-amount (stream-id uint))
    (let (
            (stream (unwrap! (get-stream stream-id) ERR-STREAM-NOT-FOUND))
            (current-time burn-block-height)
        )
        (if (get is-active stream)
            (let (
                    (elapsed-time (- current-time (get start-time stream)))
                    (total-duration (- (get end-time stream) (get start-time stream)))
                    (total-amount (get total-amount stream))
                )
                (if (>= current-time (get end-time stream))
                    (ok (- total-amount (get amount-paid stream)))
                    (ok (/ (* elapsed-time total-amount) total-duration))
                )
            )
            (ok u0)
        )
    )
)

(define-public (deposit)
    (let ((amount (stx-get-balance tx-sender)))
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set employer-balances { employer: tx-sender } { balance: (+ (get balance (get-employer-balance tx-sender)) amount) })
        (ok amount)
    )
)

(define-public (create-stream
        (employee principal)
        (total-amount uint)
        (duration uint)
    )
    (let (
            (employer-balance (get balance (get-employer-balance tx-sender)))
            (start-time burn-block-height)
            (end-time (+ burn-block-height duration))
            (stream-id (+ (var-get stream-nonce) u1))
        )
        (asserts! (>= employer-balance total-amount) ERR-INSUFFICIENT-BALANCE)
        (asserts! (> duration u0) ERR-INVALID-TIME)
        (map-set salary-streams { stream-id: stream-id } {
            employer: tx-sender,
            employee: employee,
            total-amount: total-amount,
            start-time: start-time,
            end-time: end-time,
            amount-paid: u0,
            is-active: true,
        })
        (map-set employer-balances { employer: tx-sender } { balance: (- employer-balance total-amount) })
        (var-set stream-nonce stream-id)
        (ok stream-id)
    )
)

(define-public (withdraw (stream-id uint))
    (let (
            (stream (unwrap! (get-stream stream-id) ERR-STREAM-NOT-FOUND))
            (available-amount (unwrap! (calculate-streamed-amount stream-id) ERR-STREAM-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get employee stream)) ERR-NOT-AUTHORIZED)
        (asserts! (get is-active stream) ERR-STREAM-ENDED)
        (asserts! (> available-amount u0) ERR-INSUFFICIENT-BALANCE)
        (try! (as-contract (stx-transfer? available-amount tx-sender (get employee stream))))
        (map-set salary-streams { stream-id: stream-id }
            (merge stream {
                amount-paid: (+ (get amount-paid stream) available-amount),
                is-active: (< burn-block-height (get end-time stream)),
            })
        )
        (ok available-amount)
    )
)
