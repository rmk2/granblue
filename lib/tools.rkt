#lang racket

(require db)

(require "data.rkt")
(require "sql.rkt")
(require "structs.rkt")

(provide (all-defined-out))

(define (dev-cleanup)
  (begin
    (query-exec sqlc "DROP TABLE IF EXISTS accountDB, characterDB, summonDB, elementDB, raceDB, rarityDB, typeDB, weaponDB")
    (query-exec sqlc "DROP VIEW IF EXISTS characterView, summonView")))

(define (dev-setup)
  (dev-cleanup)
  (sql-granblue-create-accountdb)
  ;; (sql-granblue-update-accountdb
  ;;  (append (map (lambda (x) (granblue-account null (format "granblue+~a@rmk2.org" x) 0 0 1)) (range 1 11))
  ;; 	   (map (lambda (x) (granblue-account null (format "ryko+gb~a@rmk2.org" x) 0 0 1)) (range 1 20))
  ;; 	   (list (granblue-account null "granblue+1@rmk2.org" 0 1 1))
  ;; 	   (list (granblue-account null "granblue+2@rmk2.org" 0 1 1))))
  (sql-granblue-init-db)
  (sql-granblue-create-characterdb)
  (sql-granblue-create-characterdb-view)
  (sql-granblue-update-characterdb
   (sort
    (append
     (prepare-characters (get-characters "https://gbf.wiki/SSR_Characters_List") #:rarity "SSR" )
     (prepare-characters (get-characters "https://gbf.wiki/SR_Characters_List") #:rarity "SR")
     (prepare-characters (get-characters "https://gbf.wiki/R_Characters_List") #:rarity "R" ))
    string-ci<=?
    #:key (lambda (x) (granblue-character-name x))))
  (sql-granblue-create-summondb)
  (sql-granblue-create-summondb-view)
  (sql-granblue-update-summondb
   (sort
    (prepare-summons (get-summons "https://gbf.wiki/SSR_Summons_List") #:rarity "SSR")
    string-ci<=?
    #:key (lambda (x) (granblue-summon-name x))))
  (sql-granblue-create-char-associations)
  (sql-granblue-create-summon-associations))
