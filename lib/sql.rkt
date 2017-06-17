#lang racket

(require db)
(require db/util/datetime)
(require srfi/19)

(require "structs.rkt")

(provide (all-defined-out))

;; SQL connection

(define sqlc (virtual-connection
	      (connection-pool
	       (lambda ()
		 (mysql-connect #:user "granblue"
				#:database "granblue"
				#:password (getenv "MYSQL_PASSWORD")))
	       #:max-idle-connections 5)))

;; SQL Macros

(define-syntax (sql-granblue-create-table stx)
  (syntax-case stx (list)
    [(_ (a ...))
     #'(for-each
	(lambda (t)
	  (if (table-exists? sqlc (format "~aDB" t))
	      #t
	      (query-exec sqlc (format "CREATE TABLE ~aDB ( ~aID SMALLINT NOT NULL AUTO_INCREMENT, ~aName VARCHAR(64), PRIMARY KEY ( ~aID ))" t t t t))))
	(list a ...))]
    [(_ a ...) #'(sql-granblue-create-table (a ...))]))

(define-syntax (sql-granblue-update-table stx)
  (syntax-case stx ()
    [(_ table (a ...))
     (with-syntax ([make-table (format "~aDB" (syntax->datum #'table))]
		   [make-name (format "~aName" (syntax->datum #'table))])
       #'(for-each
	  (lambda (x)
	    (query-exec sqlc (format "INSERT INTO ~a VALUES (0, ?)" make-table) x))
	  (list a ...)))]
    [(_ table a ...) #'(sql-granblue-update-table table (a ...))]
    [(_ table #:items a ...) #'(sql-granblue-update-table table (a ...))]))

;; SQL name -> ID

(define/contract (sql-granblue-query-id table query)
  (string? string? . -> . number?)
  (let ([result (query-maybe-value sqlc (format "SELECT ~aID FROM ~aDB WHERE ~aName = ?" table table table) query)])
    (if (false? result) 0 result)))

;; SQL association lists

(define/contract (sql-granblue-query-assoc table)
  (string? . -> . list?)
  (let ([table (string-downcase table)])
    (map vector->list (query-rows sqlc (format "SELECT DISTINCT ~aName,~aID FROM ~aDB" table table table)))))

;; (define rarity-list (sql-granblue-query-assoc "rarity"))
;; (define element-list (sql-granblue-query-assoc "element"))
;; (define type-list (sql-granblue-query-assoc "type"))
;; (define race-list (sql-granblue-query-assoc "race"))
;; (define weapon-list (sql-granblue-query-assoc "weapon"))

;; SQL tables + functions

(define (sql-granblue-create-accountdb)
  (if (table-exists? sqlc "accountDB")
      #t
      (query-exec sqlc "CREATE TABLE accountDB ( accountID SMALLINT NOT NULL AUTO_INCREMENT, email VARCHAR(64) NOT NULL, init SMALLINT NOT NULL DEFAULT 1, played SMALLINT NOT NULL DEFAULT 0, display SMALLINT NOT NULL DEFAULT 1, PRIMARY KEY ( accountID ), UNIQUE KEY ( email ))")))

(define (sql-granblue-update-accountdb lst)
  (for-each (lambda (x)
	      (query sqlc "INSERT INTO accountDB VALUES (0, ?, ?, ?, ?) ON DUPLICATE KEY UPDATE init=?,played=?,display=?"
		     (granblue-account-email x)
		     (granblue-account-init x)
		     (granblue-account-played x)
		     (granblue-account-display x)
		     (granblue-account-init x)
		     (granblue-account-played x)
		     (granblue-account-display x)))
	    lst))

(define (sql-granblue-create-characterdb)
  (if (table-exists? sqlc "characterDB")
      #t
      (query-exec sqlc "CREATE TABLE characterDB ( characterID SMALLINT NOT NULL AUTO_INCREMENT, characterName VARCHAR(64) NOT NULL, rarityID SMALLINT NOT NULL, elementID SMALLINT NOT NULL, typeID SMALLINT NOT NULL, raceID SMALLINT NOT NULL, weaponID SMALLINT, rating SMALLINT, PRIMARY KEY ( characterID ), UNIQUE KEY ( characterName, rarityID, elementID ))")))

(define (sql-granblue-update-characterdb lst)
  (for-each (lambda (x)
	      (query sqlc "INSERT INTO characterDB VALUES (0, ?, ?, ?, ?, ?, ?, ?) ON DUPLICATE KEY UPDATE rating=?"
		     (granblue-character-name x)
		     (sql-granblue-query-id "rarity" (granblue-character-rarity x))
		     (sql-granblue-query-id "element" (granblue-character-element x))
		     (sql-granblue-query-id "type" (granblue-character-type x))
		     (sql-granblue-query-id "race" (granblue-character-race x))
		     (sql-granblue-query-id "weapon" (granblue-character-weapon x))
		     (granblue-character-rating x)
		     (granblue-character-rating x)))
	    lst))

(define (sql-granblue-create-characterdb-view)
  (if (table-exists? sqlc "characterView")
      #t
      (query-exec sqlc (string-append "CREATE VIEW characterView AS "
				      "SELECT "
				      "characterID,characterName,rarity.rarityName,element.elementName,"
				      "type.typeName,race.raceName,weapon.weaponName,rating "
				      "FROM characterDB "
				      "LEFT JOIN rarityDB AS rarity ON rarity.rarityID = characterDB.rarityID "
				      "LEFT JOIN elementDB AS element ON element.elementID = characterDB.elementID "
				      "LEFT JOIN typeDB AS type ON type.typeID = characterDB.typeID "
				      "LEFT JOIN raceDB AS race ON race.raceID = characterDB.raceID "
				      "LEFT JOIN weaponDB AS weapon ON weapon.weaponID = characterDB.weaponID"))))

(define (sql-granblue-create-summondb)
  (if (table-exists? sqlc "summonDB")
      #t
      (query-exec sqlc "CREATE TABLE summonDB ( summonID SMALLINT NOT NULL AUTO_INCREMENT, summonName VARCHAR(64) NOT NULL, rarityID SMALLINT NOT NULL, elementID SMALLINT NOT NULL, PRIMARY KEY ( summonID ), UNIQUE KEY ( summonName, rarityID, elementID ))")))

(define (sql-granblue-update-summondb lst)
  (for-each (lambda (x)
	      (query-exec sqlc "INSERT IGNORE INTO summonDB VALUES (0, ?, ?, ?)"
			  (granblue-summon-name x)
			  (sql-granblue-query-id "rarity" (granblue-summon-rarity x))
			  (string->number (granblue-summon-element x))))
	    lst))

(define (sql-granblue-create-summondb-view)
  (if (table-exists? sqlc "summonView")
      #t
      (query-exec sqlc (string-append "CREATE VIEW summonView AS "
				      "SELECT "
				      "summonID,summonName,rarity.rarityName,element.elementName "
				      "FROM summonDB "
				      "LEFT JOIN rarityDB AS rarity ON rarity.rarityID = summonDB.rarityID "
				      "LEFT JOIN elementDB AS element ON element.elementID = summonDB.elementID"))))

;; SQL account associations

(define (sql-granblue-create-char-associations)
  (if (table-exists? sqlc "accountCharAssociations")
      #t
      (query-exec sqlc "CREATE TABLE accountCharAssociations ( accountID SMALLINT NOT NULL, characterID SMALLINT NOT NULL, datetime DATETIME, UNIQUE KEY ( accountID, characterID ))")))

(define (sql-granblue-update-char-associations lst)
  (for-each (lambda (x)
	      (query-exec sqlc "INSERT INTO accountCharAssociations VALUES (?, ?, ?) ON DUPLICATE KEY UPDATE datetime=?"
			  (granblue-char-association-account x)
			  (granblue-char-association-character x)
			  (granblue-char-association-datetime x)
			  (granblue-char-association-datetime x)))
	    lst))

(define (sql-granblue-get-char-associations [arg "%"])
  (query-rows sqlc "SELECT db.*,cv.characterName,cv.rarityName,cv.elementName,cv.typeName,cv.raceName,cv.weaponName FROM accountDB AS db RIGHT JOIN accountCharAssociations AS ca ON db.accountID = ca.accountID LEFT JOIN characterView AS cv ON cv.characterID = ca.characterID WHERE display = 1 AND db.email LIKE ? OR db.accountID LIKE ?" arg arg))

(define/contract (association-name->id lst #:type [type "character"])
  ((listof struct?) #:type string?  . -> . (listof struct?))
  (map (lambda (entry)
	 (cond [(equal? type "character")
		(granblue-char-association
		 (granblue-char-association-account entry)
		 (sql-granblue-query-id "character" (granblue-char-association-character entry)))]
	       [(equal? type "summon")
		(granblue-summon-association
		 (granblue-summon-association-account entry)
		 (sql-granblue-query-id "summon" (granblue-summon-association-summon entry)))]))
       lst))

(define (sql-granblue-create-summon-associations)
  (if (table-exists? sqlc "accountSummonAssociations")
      #t
      (query-exec sqlc "CREATE TABLE accountSummonAssociations ( accountID SMALLINT NOT NULL, summonID SMALLINT NOT NULL, datetime DATETIME, UNIQUE KEY ( accountID, summonID ))")))

(define (sql-granblue-update-summon-associations lst)
  (for-each (lambda (x)
	      (query-exec sqlc "INSERT IGNORE INTO accountSummonAssociations VALUES (?, ?, ?) ON DUPLICATE KEY UPDATE datetime=?"
			  (granblue-summon-association-account x)
			  (granblue-summon-association-summon x)
			  (granblue-summon-association-datetime x)
			  (granblue-summon-association-datetime x)))
	    lst))

(define (sql-granblue-get-summon-associations [arg "%"])
  (query-rows sqlc "SELECT db.*,sv.summonName,sv.rarityName,sv.elementName FROM accountDB AS db RIGHT JOIN accountSummonAssociations AS sa ON db.accountID = sa.accountID LEFT JOIN summonView AS sv ON sv.summonID = sa.summonID WHERE display = 1 AND db.email LIKE ? OR db.accountID LIKE ?" arg arg))

;; Initial SQL data

(define (sql-granblue-init-db)
  (sql-granblue-create-table "element" "race" "rarity" "type" "weapon")
  (sql-granblue-update-table "element" "fire" "wind" "earth" "water" "light" "dark" "any")
  (sql-granblue-update-table "rarity" "n" "r" "sr" "ssr")
  (sql-granblue-update-table "race" "human" "erune" "harvin" "draph" "primal" "unknown")
  (sql-granblue-update-table "type" "attack" "defense" "balanced" "heal" "special")
  (sql-granblue-update-table "weapon" "sabre" "melee" "staff" "harp" "dagger" "gun" "bow" "spear" "axe" "human" "katana"))
