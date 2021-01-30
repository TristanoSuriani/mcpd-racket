#lang racket

;;; Defines the chip registry's operations.

(provide
 ; [Function] Creates a new registry.
 ; -> empty list
 new-registry

 ; [Function] Registers a chip in the registry.
 ; number, list -> list
 chip-register

 ; [Function] Unregisters a chip in the registry.
 ; number, list -> list
 chip-unregister

 ; [Function] Checks if a chip is already registered in a registry.
 ; number, list -> boolean
 chip-registered?)

(require rackunit)

; ------------------------
; implementation

(define (new-registry)
  '())

(define (chip-register cat-id registry)
  (append registry (list cat-id)))

(define (chip-unregister cat-id registry)
  (remove cat-id registry))

(define (chip-registered? cat-id registry)
  (member? cat-id registry))

; Utility function to know if an element belongs to a list.
; atom, list -> boolean
(define (member? element lst)
  (if [member element lst] #t #f))


; ------------------------
; unit tests

(check-equal?
 (new-registry)
 '())

(check-equal?
 (chip-registered? 1 (chip-register 1 (new-registry)))
 #t)

(check-equal?
 (chip-registered? 1 (chip-unregister 1 (chip-register 1 (new-registry))))
 #f)