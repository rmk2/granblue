#lang racket

(provide (all-defined-out))

;; Turn human-readable modifiers into usable form, e.g. 80(%) -> 0.8

(define/contract (modifier->percent mod)
  (-> real? real?)
  (+ 1 (/ mod 100)))

(define/contract (percent->modifier mod)
  (-> real? real?)
  (- (* mod 100) 100))

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

(define/contract (compare-summons #:normal [normal 1] #:magna [magna 1] #:unknown [unknown 1] #:order [order 'desc])
  (->* () (#:normal real? #:magna real? #:unknown real? #:order (and/c symbol? (or/c 'asc 'desc))) (listof pair?))
  (let ([low0 (grid-modifier ((percent->modifier (* normal magna unknown))) (40))]
	[low3 (grid-modifier ((percent->modifier (* normal magna unknown))) (50))]
	[low4 (grid-modifier ((percent->modifier (* normal magna unknown))) (60))]
	[mid3 (grid-modifier ((percent->modifier (* normal magna unknown))) (80))]
	[high0 (grid-modifier ((percent->modifier (* normal magna unknown))) (120))]
	[high4 (grid-modifier ((percent->modifier (* normal magna unknown))) (140))]
	[omega0 (* normal (grid-modifier ((percent->modifier magna) . 50)) unknown)]
	[omega4 (* normal (grid-modifier ((percent->modifier magna) . 100)) unknown)])
    (sort (map (lambda (value label) (cons label value))
	       (list low0 low3 low4 mid3 high0 high4 omega0 omega4)
	       (list "Low 0*" "Low 3*/Mid 0*" "Low 4*" "Mid 3*/Mid 4*" "High 0*" "High 4*" "Omega 0*" "Omega 4*"))
	  (if (equal? order 'desc) >= <=)
	  #:key cdr)))

;; Shortcuts for common weapon types

(define/contract (unknown-large level)
  (-> (between/c 1 10) number?)
  (weapon-modifier level #:base 5))

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
  (-> (between/c 1 10) number?)
  (weapon-modifier level #:base 2))

(define/contract (normal-large level)
  (-> (between/c 1 15) number?)
  (weapon-modifier level #:base 5 #:modifier 0.6))

(define/contract (normal2-large level)
  (-> (between/c 1 15) number?)
  (weapon-modifier level #:base 6 #:modifier 0.8))
