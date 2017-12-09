#lang sicp

(#%require "common.scm")

;   Exercise 3.26
;   =============
;   
;   To search a table as implemented above, one needs to scan through the
;   list of records.  This is basically the unordered list representation of
;   section [2.3.3].  For large tables, it may be more efficient to
;   structure the table in a different manner.  Describe a table
;   implementation where the (key, value) records are organized using a
;   binary tree, assuming that keys can be ordered in some way (e.g.,
;   numerically or alphabetically).  (Compare exercise [2.66] of chapter 2.)
;   
;   ------------------------------------------------------------------------
;   [Exercise 3.26]: http://sicp-book.com/book-Z-H-22.html#%_thm_3.26
;   [Section 2.3.3]: http://sicp-book.com/book-Z-H-16.html#%_sec_2.3.3
;   [Exercise 2.66]: http://sicp-book.com/book-Z-H-22.html#%_thm_2.66
;   3.3.3 Representing Tables - p272
;   ------------------------------------------------------------------------

(-start- "3.26")

; As 'table' is referring to the n-dimensional store need a different name
; for the underlying store - the best I can come up with is 'dimension'.

;(define (new-table-assoc) (list '*table*))



(define (try-get-record-assoc table key)
  (assoc key (cdr table)))

(define (get-record-assoc table key)
  (let ((entry (try-get-record-assoc table key)))
    (cond (entry entry)
           (else
            (let ((record (list key)))
              (set-cdr! table
                        (cons record
                              (cdr table)))
              record)))))

(define (update-record-assoc record value)
  (set-cdr! record value))

    
(define (make-table)
  (make-table-generic try-get-record-assoc
                      get-record-assoc
                      update-record-assoc))


(define (make-table-generic try-get-record get-record update-record)
  (let
      ((local-table (list '*table*)))
    (define (lookup table keys)
      (if (null? keys)
          (error "Was given NULL keys"))
      (let ((entry (try-get-record table (car keys))) 
            (next-keys (cdr keys)))
        (if  entry
             ; if next-keys is empty then at a 'leaf'
             ;   otherwise we have a subtable
             (if (null? next-keys)
                 (cdr entry)
                 (lookup entry next-keys))
             false)))
    (define (insert! table keys value)
     (if (null? keys)
          (error "Was given NULL keys"))          
      (let ((entry (get-record table (car keys)))
            (key (car keys))
            (next-keys (cdr keys)))
        (if (null? next-keys)
            ; at 'leaf' - entry is a record
            (update-record entry value)               
            ;handle a subltable
            (insert! entry next-keys value)))
      'ok)
         
    (define (dispatch m)
      (cond ((eq? m 'lookup)
             (lambda (keys) (lookup local-table keys)))
            ((eq? m 'insert!)
             (lambda (keys value) (insert! local-table keys value)))
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))



(define table (make-table))

(prn "" "Adding 'apple and 'ant under single key.")  
(define key1a '('a))
(define key1b '(1))
(ignore ((table 'insert!) key1a 'apple))
(ignore ((table 'insert!) key1b 'ant))


(prn "" "Adding 'banana and 'bull under double keys.")  
(define key2a '('b 2))
(define key2b '('bb 22))
(ignore ((table 'insert!) key2a 'banana))
(ignore ((table 'insert!) key2b 'bull))


(prn "" "Adding 'cherry and 'cat under tripple  keys.")  
(define key3a '('c 2 'same))
(define key3b '('c 22 'same))
(ignore ((table 'insert!) key3a 'cherry))
(ignore ((table 'insert!) key3b 'cat))


(prn "" "Retrieving values:"
     ((table 'lookup) key1a)
     ((table 'lookup) key1b)
     ((table 'lookup) key2a)
     ((table 'lookup) key2b)
     ((table 'lookup) key3a)
     ((table 'lookup) key3b))



(--end-- "3.26")

