#lang racket

;;; Defines the chip registry's operations.

(provide
 new-registry register-chip unregister-chip factory-reset registered?)

(define (new-registry)
  '())

(define (register-chip id-cat chips-registry)
  (append chips-registry (list id-cat)))

(define (unregister-chip id-cat chips-registry)
  (remove id-cat chips-registry))

(define (factory-reset)
  (new-registry))

(define (registered? id-cat chips-registry)
  (member? id-cat chips-registry))

; Utility function to know if an element belongs to a list
(define (member? element lst)
  (if [member element lst] #t #f))