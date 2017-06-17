#lang racket

(require net/url)
(require xml)
(require xml/path)
(require xexpr-path)
(require html-parsing)

(require "structs.rkt")

(provide (all-defined-out))

;; Get character data from gbf.wiki

(define/contract (get-characters [address "https://gbf.wiki/SSR_Characters_List"])
  (->* () (string?) (listof list?))
  (filter-not empty?
	      (map (lambda (x)
		     (remove-duplicates
		      (append (xexpr-path-list '(td a @ title) x)
			      (xexpr-path-list '(td img @ alt) x))))
		   (xexpr-path-list '(html body div div div table tr)
				    (call/input-url (string->url address)
						    get-pure-port
						    html->xexp)))))

(define/contract (prepare-characters lst #:rarity [rarity "SSR"] #:id [id 0] #:rating [rating 0])
  (->* ((listof list?))
       (#:rarity string? #:id integer? #:rating integer?)
       (listof (struct/dc granblue-character
			  [id integer?]
			  [name string?]
			  [rarity string?]
			  [element string?]
			  [type string?]
			  [race string?]
			  [weapon string?]
			  [rating integer?])))
  (define (cadr-guard arg) (cond [(false? arg) "NULL"]
				 [(pair? arg) (cadr arg)]
				 [else arg]))
  (map (lambda (input)
	 (apply granblue-character
		(map (lambda (x) (match x
				   [#f "NULL"]
				   [(regexp #rx"Category:([a-zA-Z]+) Characters" (list _ str)) str]
				   [(regexp #rx"Label Element ([a-zA-Z]+).png" (list _ str)) str]
				   [(regexp #rx"wikipedia:.*") "NULL"]
				   [else x]))
		     (list id
			   (cadr-guard (first input))
			   rarity
			   (cadr-guard (assoc 'alt input))
			   (cadr-guard (second input))
			   (cadr-guard (third input))
			   (if (>= (length input) 4)
			       (cadr-guard (fourth input))
			       "")
			   rating))))
       (filter (lambda (x) (assoc 'alt x)) lst)))

;; Get summon data from gbf.wiki

(define/contract (get-summons [address "https://gbf.wiki/SSR_Summons_List"])
  (->* () (string?) (listof list?))
  (filter-not empty?
	      (map (lambda (x)
		     (list (xexpr-path-first '(td a @ title) x)
			   (xexpr-path-first '(td span) x)))
		   (xexpr-path-list '(html body div div div table tr)
				    (call/input-url (string->url address)
						    get-pure-port
						    html->xexp)))))

(define/contract (prepare-summons lst #:rarity [rarity "SSR"] #:id [id 0])
  (->* ((listof list?))
       (#:rarity string? #:id positive-integer?)
       (listof (struct/dc granblue-summon
			  [id integer?]
			  [name string?]
			  [rarity string?]
			  [element string?])))
  (filter-map (lambda (x)
		(match x
		  [(list (list 'title (pregexp "^Delay|Drain|Damage Formula|SideM")) (list 'span _ element)) #f]
		  [(list (list 'title name) (list 'span _ element))
		   (granblue-summon id (string-replace (string-replace name "=" "") "&#39;" "'") rarity element)]
		  [else #f]))
	      lst))
