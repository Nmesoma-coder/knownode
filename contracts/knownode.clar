;; KnowNode - Decentralized Education Credentials
;; Handles peer assessment and skill verification on Stacks blockchain

;; Constants
(define-constant contract-owner tx-sender)
(define-constant min-assessors u3)
(define-constant assessment-threshold u70)  ;; 70% approval needed
(define-constant max-assessors u20)  ;; Maximum number of assessors per skill

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
        reputation: uint
    }
)

(define-map skills 
    uint 
    {
        name: (string-ascii 50),
        description: (string-ascii 200),
        required-assessments: uint
    }
)

(define-map skill-assessments
    {skill-id: uint, user: principal}
    {
        assessors: (list 20 principal),
        scores: (list 20 uint),
        verified: bool,
        timestamp: uint
    }
)

;; Data var for skill ID counter
(define-data-var skill-id-counter uint u0)

;; Input validation functions
(define-private (is-valid-skill-id (skill-id uint))
    (and 
        (>= skill-id u0)
        (< skill-id (var-get skill-id-counter))
    )
)

(define-private (is-valid-string (str (string-ascii 200)))
    (and 
        (not (is-eq str ""))
        (<= (len str) u200)
    )
)

;; Public functions

;; Register a new user
(define-public (register-user)
    (let ((sender tx-sender))
        (asserts! (not (default-to false (get registered (map-get? users sender)))) err-already-registered)
        (ok (map-set users 
            sender
            {
                registered: true,
                skills: (list ),
                reputation: u0
            }
        ))
    )
)

;; Add a new skill to the platform
(define-public (add-skill (name (string-ascii 50)) (description (string-ascii 200)) (required-assessments uint))
    (let ((skill-id (get-next-skill-id)))
        ;; Input validation
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (is-valid-string name) err-invalid-input)
        (asserts! (is-valid-string description) err-invalid-input)
        (asserts! (<= required-assessments max-assessors) err-invalid-score)
        (asserts! (> required-assessments u0) err-invalid-input)
        
        (ok (map-set skills 
            skill-id
            {
                name: name,
                description: description,
                required-assessments: required-assessments
            }
        ))
    )
)

;; Request skill assessment
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
                timestamp: block-height
            }
        ))
    )
)

;; Submit assessment for a user's skill
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
        
        ;; Check if append would exceed max length before creating new lists
        (asserts! (< (len current-assessors) u20) err-max-assessors-reached)
        (asserts! (< (len current-scores) u20) err-max-assessors-reached)
        
        ;; Create new assessors and scores lists
        (let (
            (new-assessors (unwrap! (as-max-len? (append current-assessors sender) u20) err-max-assessors-reached))
            (new-scores (unwrap! (as-max-len? (append current-scores score) u20) err-max-assessors-reached))
        )
            ;; Update assessment
            (ok (map-set skill-assessments
                {skill-id: skill-id, user: user}
                (merge assessment {
                    assessors: new-assessors,
                    scores: new-scores
                })
            ))
        )
    )
)

;; Finalize skill verification
(define-public (finalize-verification (skill-id uint))
    (let (
        (sender tx-sender)
        (assessment (unwrap! (map-get? skill-assessments {skill-id: skill-id, user: sender}) err-not-registered))
        (scores (get scores assessment))
        (assessor-count (len (get assessors assessment)))
        )
        ;; Input validation
        (asserts! (is-valid-skill-id skill-id) err-invalid-skill-id)
        (asserts! (>= assessor-count min-assessors) err-insufficient-assessors)
        
        ;; Calculate average score
        (let (
            (total-score (fold + scores u0))
            (average-score (/ total-score assessor-count))
            )
            
            ;; Update assessment status if threshold is met
            (ok (map-set skill-assessments
                {skill-id: skill-id, user: sender}
                (merge assessment {
                    verified: (>= average-score assessment-threshold)
                })
            ))
        )
    )
)

;; Read-only functions

;; Get user details
(define-read-only (get-user-details (user principal))
    (map-get? users user)
)

;; Get skill details
(define-read-only (get-skill-details (skill-id uint))
    (map-get? skills skill-id)
)

;; Get assessment details
(define-read-only (get-assessment-details (skill-id uint) (user principal))
    (map-get? skill-assessments {skill-id: skill-id, user: user})
)

;; Get current assessor count for a skill assessment
(define-read-only (get-assessor-count (skill-id uint) (user principal))
    (len (get assessors (unwrap! (map-get? skill-assessments {skill-id: skill-id, user: user}) u0)))
)

;; Private functions

;; Get next skill ID (internal counter)
(define-private (get-next-skill-id)
    (let ((current (var-get skill-id-counter)))
        (var-set skill-id-counter (+ current u1))
        current
    )
)