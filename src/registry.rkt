#lang racket

;;; Defines the chip registry's operations.

(provide
 ; [Function] Creates a new registry
 ; -> empty list
 new-registry

 ; [Function] Registers a chip in the registry
 ; number, list -> list
 chip-register

 ; [Function] Unregisters a chip in the registry
 ; number, list -> list
 chip-unregister

 ; [Function] Checks if a chip is already registered in a registry
 ; number, list -> boolean
 chip-registered?)


; ------------------------
; implementation

(define (new-registry)
  '())

(define (chip-register cat-id chips-registry)
  (append chips-registry (list cat-id)))

(define (chip-unregister cat-id chips-registry)
  (remove cat-id chips-registry))

(define (chip-registered? cat-id chips-registry)
  (member? cat-id chips-registry))

; Utility function to know if an element belongs to a list
(define (member? element lst)
  (if [member element lst] #t #f))
