;; EduRank - Blockchain-powered Reputation System for Educational Resources
;; Rate and review educational resources with transparent reputation tracking

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u101))
(define-constant err-already-reviewed (err u102))
(define-constant err-invalid-rating (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-already-voted (err u105))
(define-constant err-invalid-category (err u106))
(define-constant err-resource-flagged (err u107))
(define-constant err-insufficient-reputation (err u108))

;; Data Variables
(define-data-var resource-nonce uint u0)
(define-data-var review-nonce uint u0)
(define-data-var category-nonce uint u0)

;; Data Maps
(define-map resources
    uint
    {
        creator: principal,
        title: (string-ascii 100),
        category: (string-ascii 50),
        resource-uri: (string-ascii 256),
        total-reviews: uint,
        total-rating: uint,
        created-at: uint,
        upvotes: uint,
        downvotes: uint,
        is-flagged: bool,
        view-count: uint
    }
)

(define-map reviews
    uint
    {
        resource-id: uint,
        reviewer: principal,
        rating: uint,
        comment: (string-ascii 256),
        timestamp: uint,
        helpful-count: uint,
        unhelpful-count: uint,
        is-verified: bool
    }
)

(define-map user-reviews
    { user: principal, resource-id: uint }
    uint
)

(define-map resource-reviews
    uint
    (list 200 uint)
)

(define-map user-reputation
    principal
    {
        total-reviews: uint,
        total-helpful-marks: uint,
        reputation-score: uint,
        badge-level: uint,
        resources-created: uint
    }
)

(define-map resource-votes
    { user: principal, resource-id: uint }
    { vote-type: (string-ascii 10) }
)

(define-map review-helpfulness
    { user: principal, review-id: uint }
    { is-helpful: bool }
)

(define-map categories
    uint
    {
        name: (string-ascii 50),
        description: (string-ascii 256),
        resource-count: uint
    }
)

(define-map moderators
    principal
    bool
)

;; Public Functions

;; Register a new educational resource
;; #[allow(unchecked_data)]
(define-public (register-resource (title (string-ascii 100)) (category (string-ascii 50)) (resource-uri (string-ascii 256)))
    (let
        (
            (resource-id (var-get resource-nonce))
            (user-rep (default-to { total-reviews: u0, total-helpful-marks: u0, reputation-score: u0, badge-level: u0, resources-created: u0 }
                                   (map-get? user-reputation tx-sender)))
        )
        (map-set resources resource-id {
            creator: tx-sender,
            title: title,
            category: category,
            resource-uri: resource-uri,
            total-reviews: u0,
            total-rating: u0,
            created-at: stacks-block-height,
            upvotes: u0,
            downvotes: u0,
            is-flagged: false,
            view-count: u0
        })
        (map-set user-reputation tx-sender 
            (merge user-rep { resources-created: (+ (get resources-created user-rep) u1) }))
        (var-set resource-nonce (+ resource-id u1))
        (ok resource-id)
    )
)

;; Submit a review for a resource
;; #[allow(unchecked_data)]
(define-public (submit-review (resource-id uint) (rating uint) (comment (string-ascii 256)))
    (let
        (
            (resource (unwrap! (map-get? resources resource-id) err-not-found))
            (review-id (var-get review-nonce))
            (existing-review (map-get? user-reviews { user: tx-sender, resource-id: resource-id }))
            (resource-review-list (default-to (list) (map-get? resource-reviews resource-id)))
            (user-rep (default-to { total-reviews: u0, total-helpful-marks: u0, reputation-score: u0, badge-level: u0, resources-created: u0 }
                                   (map-get? user-reputation tx-sender)))
        )
        (asserts! (is-none existing-review) err-already-reviewed)
        (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-rating)
        (asserts! (not (get is-flagged resource)) err-resource-flagged)
        
        (map-set reviews review-id {
            resource-id: resource-id,
            reviewer: tx-sender,
            rating: rating,
            comment: comment,
            timestamp: stacks-block-height,
            helpful-count: u0,
            unhelpful-count: u0,
            is-verified: false
        })
        
        (map-set user-reviews { user: tx-sender, resource-id: resource-id } review-id)
        (map-set resource-reviews resource-id (unwrap-panic (as-max-len? (append resource-review-list review-id) u200)))
        
        (map-set resources resource-id (merge resource {
            total-reviews: (+ (get total-reviews resource) u1),
            total-rating: (+ (get total-rating resource) rating)
        }))
        
        (map-set user-reputation tx-sender 
            (merge user-rep { 
                total-reviews: (+ (get total-reviews user-rep) u1),
                reputation-score: (+ (get reputation-score user-rep) u10)
            }))
        
        (var-set review-nonce (+ review-id u1))
        (ok review-id)
    )
)

;; Increment resource view count
;; #[allow(unchecked_data)]
(define-public (increment-view (resource-id uint))
    (let
        (
            (resource (unwrap! (map-get? resources resource-id) err-not-found))
        )
        (ok (map-set resources resource-id 
            (merge resource { view-count: (+ (get view-count resource) u1) })))
    )
)

;; Mark review as helpful
;; #[allow(unchecked_data)]
(define-public (mark-helpful (review-id uint))
    (let
        (
            (review (unwrap! (map-get? reviews review-id) err-not-found))
            (existing-vote (map-get? review-helpfulness { user: tx-sender, review-id: review-id }))
            (reviewer-rep (default-to { total-reviews: u0, total-helpful-marks: u0, reputation-score: u0, badge-level: u0, resources-created: u0 }
                                       (map-get? user-reputation (get reviewer review))))
        )
        (asserts! (is-none existing-vote) err-already-voted)
        
        (map-set reviews review-id 
            (merge review { helpful-count: (+ (get helpful-count review) u1) }))
        
        (map-set review-helpfulness { user: tx-sender, review-id: review-id } { is-helpful: true })
        
        (map-set user-reputation (get reviewer review)
            (merge reviewer-rep { 
                total-helpful-marks: (+ (get total-helpful-marks reviewer-rep) u1),
                reputation-score: (+ (get reputation-score reviewer-rep) u5)
            }))
        
        (ok true)
    )
)

;; Mark review as unhelpful
;; #[allow(unchecked_data)]
(define-public (mark-unhelpful (review-id uint))
    (let
        (
            (review (unwrap! (map-get? reviews review-id) err-not-found))
            (existing-vote (map-get? review-helpfulness { user: tx-sender, review-id: review-id }))
        )
        (asserts! (is-none existing-vote) err-already-voted)
        
        (map-set reviews review-id 
            (merge review { unhelpful-count: (+ (get unhelpful-count review) u1) }))
        
        (map-set review-helpfulness { user: tx-sender, review-id: review-id } { is-helpful: false })
        
        (ok true)
    )
)

;; Upvote a resource
;; #[allow(unchecked_data)]
(define-public (upvote-resource (resource-id uint))
    (let
        (
            (resource (unwrap! (map-get? resources resource-id) err-not-found))
            (existing-vote (map-get? resource-votes { user: tx-sender, resource-id: resource-id }))
        )
        (asserts! (is-none existing-vote) err-already-voted)
        
        (map-set resources resource-id 
            (merge resource { upvotes: (+ (get upvotes resource) u1) }))
        
        (map-set resource-votes { user: tx-sender, resource-id: resource-id } { vote-type: "upvote" })
        
        (ok true)
    )
)

;; Downvote a resource
;; #[allow(unchecked_data)]
(define-public (downvote-resource (resource-id uint))
    (let
        (
            (resource (unwrap! (map-get? resources resource-id) err-not-found))
            (existing-vote (map-get? resource-votes { user: tx-sender, resource-id: resource-id }))
        )
        (asserts! (is-none existing-vote) err-already-voted)
        
        (map-set resources resource-id 
            (merge resource { downvotes: (+ (get downvotes resource) u1) }))
        
        (map-set resource-votes { user: tx-sender, resource-id: resource-id } { vote-type: "downvote" })
        
        (ok true)
    )
)

;; Flag a resource for moderation
;; #[allow(unchecked_data)]
(define-public (flag-resource (resource-id uint))
    (let
        (
            (resource (unwrap! (map-get? resources resource-id) err-not-found))
            (user-rep (default-to { total-reviews: u0, total-helpful-marks: u0, reputation-score: u0, badge-level: u0, resources-created: u0 }
                                   (map-get? user-reputation tx-sender)))
        )
        (asserts! (>= (get reputation-score user-rep) u50) err-insufficient-reputation)
        
        (ok (map-set resources resource-id 
            (merge resource { is-flagged: true })))
    )
)

;; Unflag a resource (moderator only)
;; #[allow(unchecked_data)]
(define-public (unflag-resource (resource-id uint))
    (let
        (
            (resource (unwrap! (map-get? resources resource-id) err-not-found))
            (is-mod (default-to false (map-get? moderators tx-sender)))
        )
        (asserts! (or is-mod (is-eq tx-sender contract-owner)) err-unauthorized)
        
        (ok (map-set resources resource-id 
            (merge resource { is-flagged: false })))
    )
)

;; Add moderator (contract owner only)
;; #[allow(unchecked_data)]
(define-public (add-moderator (moderator principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
        (ok (map-set moderators moderator true))
    )
)

;; Remove moderator (contract owner only)
;; #[allow(unchecked_data)]
(define-public (remove-moderator (moderator principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
        (ok (map-delete moderators moderator))
    )
)

;; Verify a review (moderator only)
;; #[allow(unchecked_data)]
(define-public (verify-review (review-id uint))
    (let
        (
            (review (unwrap! (map-get? reviews review-id) err-not-found))
            (is-mod (default-to false (map-get? moderators tx-sender)))
            (reviewer-rep (default-to { total-reviews: u0, total-helpful-marks: u0, reputation-score: u0, badge-level: u0, resources-created: u0 }
                                       (map-get? user-reputation (get reviewer review))))
        )
        (asserts! (or is-mod (is-eq tx-sender contract-owner)) err-unauthorized)
        
        (map-set reviews review-id 
            (merge review { is-verified: true }))
        
        (map-set user-reputation (get reviewer review)
            (merge reviewer-rep { reputation-score: (+ (get reputation-score reviewer-rep) u20) }))
        
        (ok true)
    )
)

;; Update user badge level based on reputation
;; #[allow(unchecked_data)]
(define-public (update-badge-level (user principal))
    (let
        (
            (user-rep (unwrap! (map-get? user-reputation user) err-not-found))
            (new-badge-level (calculate-badge-level (get reputation-score user-rep)))
        )
        (ok (map-set user-reputation user 
            (merge user-rep { badge-level: new-badge-level })))
    )
)

;; Create a new category
;; #[allow(unchecked_data)]
(define-public (create-category (name (string-ascii 50)) (description (string-ascii 256)))
    (let
        (
            (category-id (var-get category-nonce))
        )
        (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
        
        (map-set categories category-id {
            name: name,
            description: description,
            resource-count: u0
        })
        
        (var-set category-nonce (+ category-id u1))
        (ok category-id)
    )
)

;; Update resource category
;; #[allow(unchecked_data)]
(define-public (update-resource-category (resource-id uint) (new-category (string-ascii 50)))
    (let
        (
            (resource (unwrap! (map-get? resources resource-id) err-not-found))
        )
        (asserts! (is-eq tx-sender (get creator resource)) err-unauthorized)
        
        (ok (map-set resources resource-id 
            (merge resource { category: new-category })))
    )
)

;; Delete a resource (creator or moderator only)
;; #[allow(unchecked_data)]
(define-public (delete-resource (resource-id uint))
    (let
        (
            (resource (unwrap! (map-get? resources resource-id) err-not-found))
            (is-mod (default-to false (map-get? moderators tx-sender)))
        )
        (asserts! (or (is-eq tx-sender (get creator resource)) is-mod (is-eq tx-sender contract-owner)) err-unauthorized)
        
        (ok (map-delete resources resource-id))
    )
)

;; Award reputation points manually (moderator only)
;; #[allow(unchecked_data)]
(define-public (award-reputation (user principal) (points uint))
    (let
        (
            (user-rep (default-to { total-reviews: u0, total-helpful-marks: u0, reputation-score: u0, badge-level: u0, resources-created: u0 }
                                   (map-get? user-reputation user)))
            (is-mod (default-to false (map-get? moderators tx-sender)))
        )
        (asserts! (or is-mod (is-eq tx-sender contract-owner)) err-unauthorized)
        
        (ok (map-set user-reputation user 
            (merge user-rep { reputation-score: (+ (get reputation-score user-rep) points) })))
    )
)