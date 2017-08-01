#lang racket

(require web-server/servlet
	 web-server/servlet-env
	 scribble/html)

(require db)

(require granblue)

;; URL dispatch

(define-values (main-dispatch main-url)
  (dispatch-rules
   [("accounts") (lambda (req) (exec-accounts req "%"))]
   [("accounts" (string-arg)) exec-accounts]
   [("accounts" (number-arg)) exec-accounts]))

;; Main function

(define (main req)
  (main-dispatch req))

;; Pages

(define (exec-accounts req [arg "%"])
  (define response-generator
    (response/output
     (lambda (out)
       (output-xml (doctype 'html) out)
       (output-xml
	(html
	 (head (title "Granblue Character Associations")
	       (style/inline 'type: "text/css" "table { text-align: left; border-collapse: inherit; }")
	       (style/inline 'type: "text/css" "th, td { padding: 0.35em; }")
	       (style/inline 'type: "text/css" "tr:nth-child(2n) { background-color: #efefef; }")
	       (style/inline 'type: "text/css" "th { border-bottom: 1px solid; }"))
	 (body
	  (div 'id: "content"
	       (h1 "Granblue Character Associations")
	       (div 'class: "data"
		    (call/cc
		     (lambda (return)
		       (table
			(thead
			 (tr (map th '("ID" "Email" "New?" "Played?" "Display?" "Character" "Rarity" "Element" "Type" "Race" "Weapon"))))
			(tbody
			 (map (lambda (account)
				(tr (td (vector-ref account 0))
				    (map (lambda (x) (td (match x
							   [0 "No"]
							   [1 "Yes"]
							   [(or "ssr" "sr" "r") (string-upcase x)]
							   [(pregexp "[[:alnum:]]+\\+[[:alnum:]]+@[[:alnum:]]+\\.[[:alnum:]]+") x]
							   [(pregexp "[[:alnum:]]+\\s+\\([[:alpha:]]+\\)") x]
							   [else (string-titlecase x)])))
					 (cdr (vector->list account)))))
			      (let ([data (sql-granblue-get-char-associations arg)])
				(if (empty? data)
				    (return (p "No data available!"))
				    data)))))))))))
	out))))

  (send/back response-generator))

;; Servlet

(serve/servlet main
	       #:stateless? #t
	       #:port 8000
	       #:command-line? #t
	       #:banner? #t
	       #:servlet-regexp #rx""
	       #:servlet-current-directory "/dev/shm")
