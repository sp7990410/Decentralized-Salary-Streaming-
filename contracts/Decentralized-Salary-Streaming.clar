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

(define-map stream-metrics
    { employer: principal }
    {
        total-streams-created: uint,
        total-amount-streamed: uint,
        total-amount-paid: uint,
        successful-completions: uint,
        early-terminations: uint,
        total-pause-events: uint,
    }
)

(define-data-var global-metrics {
    total-streams: uint,
    total-volume: uint,
    total-employers: uint,
    average-completion-rate: uint,
} {
    total-streams: u0,
    total-volume: u0,
    total-employers: u0,
    average-completion-rate: u0,
})

(define-read-only (get-stream (stream-id uint))
    (map-get? salary-streams { stream-id: stream-id })
)

(define-read-only (get-employer-balance (employer principal))
    (default-to { balance: u0 }
        (map-get? employer-balances { employer: employer })
    )
)

(define-read-only (get-employer-metrics (employer principal))
    (default-to {
        total-streams-created: u0,
        total-amount-streamed: u0,
        total-amount-paid: u0,
        successful-completions: u0,
        early-terminations: u0,
        total-pause-events: u0,
    }
        (map-get? stream-metrics { employer: employer })
    )
)

(define-read-only (get-global-metrics)
    (var-get global-metrics)
)

(define-private (update-employer-metrics
        (employer principal)
        (amount uint)
        (metric-type (string-ascii 20))
    )
    (let ((current-metrics (get-employer-metrics employer)))
        (map-set stream-metrics { employer: employer }
            (if (is-eq metric-type "stream-created")
                (merge current-metrics {
                    total-streams-created: (+ (get total-streams-created current-metrics) u1),
                    total-amount-streamed: (+ (get total-amount-streamed current-metrics) amount),
                })
                (if (is-eq metric-type "payment-made")
                    (merge current-metrics { total-amount-paid: (+ (get total-amount-paid current-metrics) amount) })
                    (if (is-eq metric-type "stream-completed")
                        (merge current-metrics { successful-completions: (+ (get successful-completions current-metrics) u1) })
                        (if (is-eq metric-type "stream-terminated")
                            (merge current-metrics { early-terminations: (+ (get early-terminations current-metrics) u1) })
                            (if (is-eq metric-type "stream-paused")
                                (merge current-metrics { total-pause-events: (+ (get total-pause-events current-metrics) u1) })
                                current-metrics
                            )
                        )
                    )
                )
            ))
    )
)

(define-private (update-global-metrics
        (amount uint)
        (metric-type (string-ascii 20))
    )
    (let ((current-global (var-get global-metrics)))
        (var-set global-metrics
            (if (is-eq metric-type "new-stream")
                (merge current-global {
                    total-streams: (+ (get total-streams current-global) u1),
                    total-volume: (+ (get total-volume current-global) amount),
                })
                (if (is-eq metric-type "new-employer")
                    (merge current-global { total-employers: (+ (get total-employers current-global) u1) })
                    current-global
                )
            ))
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
            (is-new-employer (is-eq (get total-streams-created (get-employer-metrics tx-sender))
                u0
            ))
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
        (update-employer-metrics tx-sender total-amount "stream-created")
        (update-global-metrics total-amount "new-stream")
        (if is-new-employer
            (update-global-metrics u0 "new-employer")
            true
        )
        (var-set stream-nonce stream-id)
        (ok stream-id)
    )
)

(define-public (withdraw (stream-id uint))
    (let (
            (stream (unwrap! (get-stream stream-id) ERR-STREAM-NOT-FOUND))
            (available-amount (unwrap! (calculate-streamed-amount stream-id) ERR-STREAM-NOT-FOUND))
            (stream-completed (>= (+ (get amount-paid stream) available-amount)
                (get total-amount stream)
            ))
        )
        (asserts! (is-eq tx-sender (get employee stream)) ERR-NOT-AUTHORIZED)
        (asserts! (get is-active stream) ERR-STREAM-ENDED)
        (asserts! (not (get is-paused stream)) ERR-STREAM-PAUSED)
        (asserts! (not (get is-terminated stream)) ERR-STREAM-TERMINATED)
        (asserts! (> available-amount u0) ERR-INSUFFICIENT-BALANCE)
        (try! (as-contract (stx-transfer? available-amount tx-sender (get employee stream))))
        (update-employer-metrics (get employer stream) available-amount
            "payment-made"
        )
        (if stream-completed
            (update-employer-metrics (get employer stream) u0 "stream-completed")
            true
        )
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
        (update-employer-metrics tx-sender u0 "stream-paused")
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
        (update-employer-metrics (get employer stream) u0 "stream-terminated")
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
