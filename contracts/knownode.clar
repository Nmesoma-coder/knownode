;; KnowNode - Decentralized Education Credentials
;; Handles peer assessment and skill verification on Stacks blockchain

;; Constants
(define-constant contract-owner tx-sender)
(define-constant min-assessors u3)
(define-constant assessment-threshold u70)  ;; 70% approval needed

;; Error codes
(define-constant err-not-authorized (err u100))
(define-constant err-already-registered (err u101))
(define-constant err-not-registered (err u102))
(define-constant err-insufficient-assessors (err u103))
(define-constant err-already-assessed (err u104))

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
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
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
        (asserts! (default-to false (get registered (map-get? users sender))) err-not-registered)
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
        )
        (asserts! (default-to false (get registered (map-get? users sender))) err-not-registered)
        (asserts! (not (is-eq sender user)) err-not-authorized)
        (asserts! (< score u101) (err u105))  ;; Score must be 0-100
        (asserts! (not (is-some (index-of (get assessors assessment) sender))) err-already-assessed)
        
        (ok (map-set skill-assessments
            {skill-id: skill-id, user: user}
            (merge assessment {
                assessors: (append (get assessors assessment) sender),
                scores: (append (get scores assessment) score)
            })
        ))
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

;; Private functions

;; Get next skill ID (internal counter)
(define-data-var skill-id-counter uint u0)

(define-private (get-next-skill-id)
    (let ((current (var-get skill-id-counter)))
        (var-set skill-id-counter (+ current u1))
        current
    )
)