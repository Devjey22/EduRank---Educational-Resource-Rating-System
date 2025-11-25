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