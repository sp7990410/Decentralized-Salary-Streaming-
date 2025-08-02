(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-STREAM-NOT-FOUND (err u101))
(define-constant ERR-INSUFFICIENT-BALANCE (err u102))
(define-constant ERR-INVALID-TIME (err u103))
(define-constant ERR-STREAM-ENDED (err u104))
(define-constant ERR-STREAM-PAUSED (err u105))
(define-constant ERR-STREAM-TERMINATED (err u106))

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
        is-paused: bool,
        pause-start-time: uint,
        total-paused-time: uint,
        is-terminated: bool,
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
        (if (and (get is-active stream) (not (get is-terminated stream)))
            (let (
                    (effective-elapsed-time (- current-time (get start-time stream)))
                    (total-paused-time (get total-paused-time stream))
                    (adjusted-elapsed-time (- effective-elapsed-time total-paused-time))
                    (total-duration (- (get end-time stream) (get start-time stream)))
                    (total-amount (get total-amount stream))
                )
                (if (>= adjusted-elapsed-time total-duration)
                    (ok (- total-amount (get amount-paid stream)))
                    (ok (/ (* adjusted-elapsed-time total-amount) total-duration))
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
            is-paused: false,
            pause-start-time: u0,
            total-paused-time: u0,
            is-terminated: false,
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
        (asserts! (not (get is-paused stream)) ERR-STREAM-PAUSED)
        (asserts! (not (get is-terminated stream)) ERR-STREAM-TERMINATED)
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

(define-public (pause-stream (stream-id uint))
    (let ((stream (unwrap! (get-stream stream-id) ERR-STREAM-NOT-FOUND)))
        (asserts! (is-eq tx-sender (get employer stream)) ERR-NOT-AUTHORIZED)
        (asserts! (get is-active stream) ERR-STREAM-ENDED)
        (asserts! (not (get is-paused stream)) ERR-STREAM-PAUSED)
        (asserts! (not (get is-terminated stream)) ERR-STREAM-TERMINATED)
        (map-set salary-streams { stream-id: stream-id }
            (merge stream {
                is-paused: true,
                pause-start-time: burn-block-height,
            })
        )
        (ok true)
    )
)

(define-public (resume-stream (stream-id uint))
    (let (
            (stream (unwrap! (get-stream stream-id) ERR-STREAM-NOT-FOUND))
            (pause-duration (- burn-block-height (get pause-start-time stream)))
        )
        (asserts! (is-eq tx-sender (get employer stream)) ERR-NOT-AUTHORIZED)
        (asserts! (get is-active stream) ERR-STREAM-ENDED)
        (asserts! (get is-paused stream) ERR-STREAM-PAUSED)
        (asserts! (not (get is-terminated stream)) ERR-STREAM-TERMINATED)
        (map-set salary-streams { stream-id: stream-id }
            (merge stream {
                is-paused: false,
                pause-start-time: u0,
                total-paused-time: (+ (get total-paused-time stream) pause-duration),
            })
        )
        (ok true)
    )
)

(define-public (terminate-stream (stream-id uint))
    (let (
            (stream (unwrap! (get-stream stream-id) ERR-STREAM-NOT-FOUND))
            (available-amount (unwrap! (calculate-streamed-amount stream-id) ERR-STREAM-NOT-FOUND))
            (remaining-amount (- (get total-amount stream) (get amount-paid stream)
                available-amount
            ))
        )
        (asserts!
            (or (is-eq tx-sender (get employer stream)) (is-eq tx-sender (get employee stream)))
            ERR-NOT-AUTHORIZED
        )
        (asserts! (get is-active stream) ERR-STREAM-ENDED)
        (asserts! (not (get is-terminated stream)) ERR-STREAM-TERMINATED)
        (if (> available-amount u0)
            (try! (as-contract (stx-transfer? available-amount tx-sender (get employee stream))))
            true
        )
        (if (> remaining-amount u0)
            (map-set employer-balances { employer: (get employer stream) } { balance: (+ (get balance (get-employer-balance (get employer stream)))
                remaining-amount
            ) }
            )
            true
        )
        (map-set salary-streams { stream-id: stream-id }
            (merge stream {
                is-terminated: true,
                is-active: false,
                is-paused: false,
                amount-paid: (+ (get amount-paid stream) available-amount),
            })
        )
        (ok {
            employee-payment: available-amount,
            employer-refund: remaining-amount,
        })
    )
)
