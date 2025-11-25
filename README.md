# EduRank - Educational Resource Reputation System

A blockchain-powered reputation system for rating and reviewing educational resources with transparent, tamper-proof records.

## Features

- Decentralized resource registration
- Transparent rating and review system
- Average rating calculation
- Review helpfulness tracking
- Prevention of duplicate reviews per user

## Smart Contract Functions

### Public Functions

- `register-resource` - Register a new educational resource
- `submit-review` - Submit a rating and review for a resource
- `mark-helpful` - Mark a review as helpful

### Read-Only Functions

- `get-resource` - Retrieve resource details
- `get-average-rating` - Calculate average rating for a resource
- `get-review` - Retrieve review details
- `get-resource-reviews` - Get all reviews for a resource
- `has-user-reviewed` - Check if user has already reviewed a resource
- `get-resource-count` - Get total resources registered

## Usage

Educators can register resources on-chain, and students can submit verified reviews with ratings from 1 to 5 stars. All reviews are permanently recorded and transparent.

## Technology Stack

- Stacks Blockchain
- Clarity Smart Contracts