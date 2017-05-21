#lang racket

(require db/util/datetime)
(require srfi/19)

(provide (all-defined-out))

(struct granblue-account (id email init played display) #:transparent)

(struct granblue-char-association (account character [datetime #:auto])
	#:transparent
	#:auto-value (srfi-date->sql-timestamp (current-date)))

(struct granblue-summon-association (account summon [datetime #:auto])
	#:transparent
	#:auto-value (srfi-date->sql-timestamp (current-date)))

(struct granblue-character (id name rarity element type race weapon rating) #:transparent)

(struct granblue-summon (id name rarity element) #:transparent)
