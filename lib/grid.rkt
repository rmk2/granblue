#lang racket

(provide (all-defined-out))

;; Turn human-readable modifiers into usable form, e.g. 80(%) -> 0.8

(define/contract (modifier->percent mod)
  (-> real? real?)
  (+ 1 (/ mod 100)))

(define/contract (percent->modifier mod)
  (-> real? real?)
  (round (- (* mod 100) 100)))

;; Calculate damage modifier for a given weapon

(define/contract (weapon-modifier [level 1] #:base [base 5] #:modifier [mod 1])
  (->* ((between/c 1 15)) (#:base natural-number/c #:modifier real?) number?)
  (let loop ([max level] [i 1] [result base])
    (if (<= i max)
	(cond [(<= i 10) (loop max (+ i 1) (+ result 1))]
	      [(<= i 15) (loop max (+ i 1) (+ result (inexact->exact mod)))])
	(exact->inexact result))))

;; Calculate grid modifier for groups of weapons. Summons can be
;; supplied as-is (e.g. (40 40) for main + support at 40% bonus
;; each).

(define-syntax (grid-modifier stx)
  (syntax-case stx (grid-modifier)
    [(_ (grid-modifier (a ...)) ...) #'(* (grid-modifier (a ...)) ...)]
    [(_ (grid-modifier (a ... . mod)) ...) #'(* (grid-modifier (a ... . mod)) ...)]
    [(_ (a ...)) #'(modifier->percent (+ a ...))]
    [(_ (a ... . mod)) #'(modifier->percent (* (+ a ...) (modifier->percent mod)))]
    [(_ (a ...) ...) #'(* (grid-modifier (a ...)) ...)]
    [(_ (a ... . mod) ...) #'(* (grid-modifier (a ... . mod)) ...)]))

;; Compare a given grid for all major damage summons

(define/contract (compare-summons #:normal [normal 1]
				  #:magna [magna 1]
				  #:unknown [unknown 1]
				  #:support [support 0]
				  #:omega? [support-omega? #f]
				  #:order [order 'desc])
  (->* () (#:normal real? #:magna real? #:unknown real? #:support real? #:omega? boolean? #:order (or/c 'asc 'desc)) (listof pair?))
  (sort
   (map (lambda (label value main-omega?)
	  (cons label
		(cond [(and support-omega? main-omega?) ;; Support Omega + Omega
		       (let ([omega-modifier (+ value support)])
			 (* normal (grid-modifier ((percent->modifier magna) . omega-modifier)) unknown))]
		      [(and (false? support-omega?) main-omega?) ;; Support Elemental + Omega
		       (* normal (grid-modifier ((percent->modifier magna) . value)) unknown (grid-modifier (support)))]
		      [(and support-omega? (false? main-omega?)) ;; Support Omega + Elemental
		       (* normal (grid-modifier ((percent->modifier magna) . support)) unknown (grid-modifier (value)))]
		      [else ;; Elemental + Elemental
		       (* normal magna unknown (grid-modifier (support value)))])))
	(list "Low 0*" "Low 3*/Mid 0*" "Low 4*" "Mid 3*/Mid 4*" "High 0*" "High 3*" "High 4*" "Omega 0*" "Omega 4*")
	(list 40 50 60 80 120 130 140 50 100)
	(list #f #f #f #f #f #f #f #t #t))
   (if (equal? order 'desc) >= <=)
   #:key cdr))

;; Shortcuts for common weapon types

(define/contract (unknown-large level)
  (-> (between/c 1 15) number?)
  (weapon-modifier level #:base 5 #:modifier 0.6))

(define/contract (unknown-massive level)
  (-> (between/c 1 15) number?)
  (weapon-modifier level #:base 8 #:modifier 1))

(define/contract (magna-small level)
  (-> (between/c 1 15) number?)
  (weapon-modifier level #:base 0 #:modifier 0.4))

(define/contract (magna-medium level)
  (-> (between/c 1 15) number?)
  (weapon-modifier level #:base 2 #:modifier 0.5))

(define/contract (magna-large level)
  (-> (between/c 1 15) number?)
  (weapon-modifier level #:base 5 #:modifier 0.6))

(define/contract (normal-small level)
  (-> (between/c 1 15) number?)
  (weapon-modifier level #:base 0 #:modifier 0.4))

(define/contract (normal-medium level)
  (-> (between/c 1 15) number?)
  (weapon-modifier level #:base 2 #:modifier 0.5))

(define/contract (normal-large level)
  (-> (between/c 1 15) number?)
  (weapon-modifier level #:base 5 #:modifier 0.6))

(define/contract (normal2-large level)
  (-> (between/c 1 15) number?)
  (weapon-modifier level #:base 6 #:modifier 0.8))
