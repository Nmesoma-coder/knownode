;; KnowNode - Decentralized Education Credentials
;; Handles peer assessment and skill verification on Stacks blockchain

;; Constants
(define-constant contract-owner tx-sender)
(define-constant min-assessors u3)
(define-constant assessment-threshold u70)  ;; 70% approval needed
(define-constant max-assessors u20)  ;; Maximum number of assessors per skill
(define-constant standard-deviation-threshold u15)  ;; 15% deviation threshold
(define-constant reputation-penalty u5)  ;; Penalty for invalid assessments
(define-constant reputation-reward u2)  ;; Reward for valid assessments

;; Error codes
(define-constant err-not-authorized (err u100))
(define-constant err-already-registered (err u101))
(define-constant err-not-registered (err u102))
(define-constant err-insufficient-assessors (err u103))
(define-constant err-already-assessed (err u104))
(define-constant err-max-assessors-reached (err u105))
(define-constant err-invalid-score (err u106))
(define-constant err-invalid-skill-id (err u107))
(define-constant err-invalid-input (err u108))

;; Data Maps
(define-map users 
    principal 
    {
        registered: bool,
        skills: (list 20 uint),
        reputation: uint,
        total-assessments: uint,
        invalid-assessments: uint
    }
)

(define-map skill-reputation
    {user: principal, skill-id: uint}
    {
        reputation: uint,
        assessments-given: uint,
        valid-assessments: uint
    }
)

(define-map skills 
    uint 
    {
        name: (string-ascii 50),
        description: (string-ascii 200),
        required-assessments: uint,
        category: (string-ascii 50)
    }
)

(define-map skill-assessments
    {skill-id: uint, user: principal}
    {
        assessors: (list 20 principal),
        scores: (list 20 uint),
        verified: bool,
        timestamp: uint,
        mean-score: uint,
        standard-deviation: uint
    }
)

;; Data var for skill ID counter
(define-data-var skill-id-counter uint u0)

;; Helper Functions for Validation
(define-private (is-valid-skill-id (skill-id uint))
    (match (map-get? skills skill-id)
        skill true
        false
    )
)

(define-private (is-valid-string (str (string-ascii 200)))
    (and 
        (not (is-eq str ""))
        (<= (len str) u200)
    )
)

;; Helper Functions for Statistical Calculations
(define-private (square (x uint))
    (* x x)
)

(define-private (calculate-mean (scores (list 20 uint)))
    (let (
        (sum (fold + scores u0))
        (count (len scores))
    )
    (if (> count u0)
        (/ sum count)
        u0
    ))
)

(define-private (square-diff-from-mean (score uint) (mean uint))
    (square (if (> score mean) 
        (- score mean)
        (- mean score)
    ))
)

(define-private (calculate-standard-deviation (scores (list 20 uint)) (mean uint))
    (let (
        (count (len scores))
        (squared-diffs (map square-diff-from-mean scores (list count mean)))
        (squared-diff-sum (fold + squared-diffs u0))
    )
    (if (> count u1)
        (sqrt (/ squared-diff-sum (- count u1)))
        u0
    ))
)

(define-private (sqrt (x uint))
    ;; Simple integer square root implementation
    (let ((guess (/ x u2)))
        (if (>= guess x)
            u1
            guess
        )
    )
)

;; Reputation Management Functions
(define-private (update-user-reputation (user principal) (skill-id uint) (is-valid bool))
    (begin
        (let (
            (current-user (unwrap! (map-get? users user) false))
            (current-skill-rep (default-to 
                {reputation: u0, assessments-given: u0, valid-assessments: u0}
                (map-get? skill-reputation {user: user, skill-id: skill-id})))
        )
            ;; Update overall user reputation
            (map-set users user
                (merge current-user {
                    reputation: (if is-valid
                        (+ (get reputation current-user) reputation-reward)
                        (if (> (get reputation current-user) reputation-penalty)
                            (- (get reputation current-user) reputation-penalty)
                            u0
                        )),
                    total-assessments: (+ (get total-assessments current-user) u1),
                    invalid-assessments: (if is-valid
                        (get invalid-assessments current-user)
                        (+ (get invalid-assessments current-user) u1)
                    )
                })
            )
            
            ;; Update skill-specific reputation
            (map-set skill-reputation
                {user: user, skill-id: skill-id}
                {
                    reputation: (if is-valid
                        (+ (get reputation current-skill-rep) reputation-reward)
                        (if (> (get reputation current-skill-rep) reputation-penalty)
                            (- (get reputation current-skill-rep) reputation-penalty)
                            u0
                        )),
                    assessments-given: (+ (get assessments-given current-skill-rep) u1),
                    valid-assessments: (if is-valid
                        (+ (get valid-assessments current-skill-rep) u1)
                        (get valid-assessments current-skill-rep)
                    )
                }
            )
        )
        true
    )
)

;; Private functions
(define-private (get-next-skill-id)
    (let ((current (var-get skill-id-counter)))
        (var-set skill-id-counter (+ current u1))
        current
    )
)

;; Helper function to process reputation updates
(define-private (process-assessor-reputation (assessor principal) (score uint) (mean uint) (std-dev uint) (skill-id uint))
    (let (
        (score-deviation (if (> score mean)
            (- score mean)
            (- mean score)
        ))
    )
        (update-user-reputation 
            assessor 
            skill-id
            (< score-deviation standard-deviation-threshold)
        )
    )
)

;; Public functions
(define-public (register-user)
    (let ((sender tx-sender))
        (asserts! (not (default-to false (get registered (map-get? users sender)))) err-already-registered)
        (ok (map-set users 
            sender
            {
                registered: true,
                skills: (list ),
                reputation: u0,
                total-assessments: u0,
                invalid-assessments: u0
            }
        ))
    )
)

(define-public (add-skill (name (string-ascii 50)) (description (string-ascii 200)) (required-assessments uint) (category (string-ascii 50)))
    (let ((skill-id (get-next-skill-id)))
        ;; Input validation
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (is-valid-string description) err-invalid-input)
        (asserts! (<= required-assessments max-assessors) err-invalid-score)
        (asserts! (> required-assessments u0) err-invalid-input)
        
        (ok (map-set skills 
            skill-id
            {
                name: name,
                description: description,
                required-assessments: required-assessments,
                category: category
            }
        ))
    )
)

(define-public (request-assessment (skill-id uint))
    (let ((sender tx-sender))
        ;; Input validation
        (asserts! (is-valid-skill-id skill-id) err-invalid-skill-id)
        (asserts! (default-to false (get registered (map-get? users sender))) err-not-registered)
        (asserts! (is-none (map-get? skill-assessments {skill-id: skill-id, user: sender})) err-already-assessed)
        
        (ok (map-set skill-assessments
            {skill-id: skill-id, user: sender}
            {
                assessors: (list ),
                scores: (list ),
                verified: false,
                timestamp: block-height,
                mean-score: u0,
                standard-deviation: u0
            }
        ))
    )
)

(define-public (submit-assessment (skill-id uint) (user principal) (score uint))
    (let (
        (sender tx-sender)
        (assessment (unwrap! (map-get? skill-assessments {skill-id: skill-id, user: user}) err-not-registered))
        (current-assessors (get assessors assessment))
        (current-scores (get scores assessment))
        )
        ;; Input validation
        (asserts! (is-valid-skill-id skill-id) err-invalid-skill-id)
        (asserts! (default-to false (get registered (map-get? users sender))) err-not-registered)
        (asserts! (not (is-eq sender user)) err-not-authorized)
        (asserts! (< score u101) err-invalid-score)
        (asserts! (not (is-some (index-of current-assessors sender))) err-already-assessed)
        (asserts! (< (len current-assessors) max-assessors) err-max-assessors-reached)
        
        ;; Check if append would exceed max length
        (asserts! (< (len current-assessors) u20) err-max-assessors-reached)
        (asserts! (< (len current-scores) u20) err-max-assessors-reached)
        
        (let (
            (new-assessors (unwrap! (as-max-len? (append current-assessors sender) u20) err-max-assessors-reached))
            (new-scores (unwrap! (as-max-len? (append current-scores score) u20) err-max-assessors-reached))
            (new-mean (calculate-mean new-scores))
        )
            ;; Update assessment with new statistical calculations
            (ok (map-set skill-assessments
                {skill-id: skill-id, user: user}
                (merge assessment {
                    assessors: new-assessors,
                    scores: new-scores,
                    mean-score: new-mean,
                    standard-deviation: (calculate-standard-deviation new-scores new-mean)
                })
            ))
        )
    )
)

(define-public (finalize-verification (skill-id uint))
    (let (
        (sender tx-sender)
        (assessment (unwrap! (map-get? skill-assessments {skill-id: skill-id, user: sender}) err-not-registered))
        (scores (get scores assessment))
        (assessors (get assessors assessment))
        (mean-score (get mean-score assessment))
        (std-dev (get standard-deviation assessment))
        (assessor-count (len assessors))
        )
        ;; Input validation
        (asserts! (is-valid-skill-id skill-id) err-invalid-skill-id)
        (asserts! (>= assessor-count min-assessors) err-insufficient-assessors)
        
        ;; Process reputation updates for all assessors
        (map process-assessor-reputation 
            assessors 
            scores
            (list assessor-count mean-score)
            (list assessor-count std-dev)
            (list assessor-count skill-id)
        )
        
        ;; Update assessment status
        (ok (map-set skill-assessments
            {skill-id: skill-id, user: sender}
            (merge assessment {
                verified: (>= mean-score assessment-threshold)
            })
        ))
    )
)

;; Read-only functions
(define-read-only (get-user-details (user principal))
    (map-get? users user)
)

(define-read-only (get-skill-details (skill-id uint))
    (map-get? skills skill-id)
)

(define-read-only (get-assessment-details (skill-id uint) (user principal))
    (map-get? skill-assessments {skill-id: skill-id, user: user})
)

(define-read-only (get-assessor-count (skill-id uint) (user principal))
    (match (map-get? skill-assessments {skill-id: skill-id, user: user})
        assessment (len (get assessors assessment))
        u0
    )
)

(define-read-only (get-user-reputation (user principal))
    (match (map-get? users user)
        user-data (get reputation user-data)
        u0
    )
)

(define-read-only (get-skill-specific-reputation (user principal) (skill-id uint))
    (get reputation (default-to 
        {reputation: u0, assessments-given: u0, valid-assessments: u0}
        (map-get? skill-reputation {user: user, skill-id: skill-id})))
)

(define-read-only (get-assessment-statistics (skill-id uint) (user principal))
    (map-get? skill-assessments {skill-id: skill-id, user: user})
)