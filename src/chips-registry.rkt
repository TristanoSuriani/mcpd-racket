#lang racket

;;; Defines the chip registry's operations.

(provide
 ; [Function] Creates a new registry
 ; -> empty list
 new-registry

 ; [Function] Registers a chip in the registry
 ; number, list -> list
 register-chip

 ; [Function] Unregisters a chip in the registry
 ; number, list -> list
 unregister-chip

 ; [Function] Checks if a chip is already registered in a registry
 ; number, list -> boolean
 registered?)


; ------------------------
; implementation

(define (new-registry)
  '())

(define (register-chip id-cat chips-registry)
  (append chips-registry (list id-cat)))

(define (unregister-chip id-cat chips-registry)
  (remove id-cat chips-registry))

(define (registered? id-cat chips-registry)
  (member? id-cat chips-registry))

; Utility function to know if an element belongs to a list
(define (member? element lst)
  (if [member element lst] #t #f))
