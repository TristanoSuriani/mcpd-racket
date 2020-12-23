#lang racket


(provide new-user-interface ui-request)

(require "model.rkt")
(require "chips-registry.rkt")
(require "interpreter.rkt")

(define-syntax ui-request
  (syntax-rules ()
    [(ui-request (handler) operation with chip id-cat)
     (handler 'operation #:id-cat id-cat)]

    [(ui-request (handler) operation)
     (handler 'operation)]))

;; Here we handle the owners's (register, unregister, reset, switch-mode) feline's requests (which are: enter, leave, and meow)

(define (new-user-interface model)
  (define registry (new-registry))
  (define current-mode "default")
  (define curfew-activated #f)
  (define modes-names (map (λ (mode) (mode-name mode)) (model-modes model)))

  (define (handle-can-do operation id-cat)
    (boolean-response->text-response (can-do? model operation id-cat registry current-mode curfew-activated)))

  (define (handle operation #:id-cat [id-cat '()])
      (cond
        ([eq? operation 'register]
         (set! registry (register-chip id-cat registry)))
        
        ([eq? operation 'unregister]
         (set! registry (unregister-chip id-cat registry)))
        
        ([eq? operation 'reset]
         (set! registry (new-registry)))
        
        ([eq? operation 'registered?]
         (registered? id-cat registry))
        
        ([eq? operation 'switch-mode]
         (let
             ([first-mode (first modes-names)])
           (begin
             (set! modes-names (append (rest modes-names) (list first-mode)))
             (set! current-mode (first modes-names))
             current-mode)))
        
        ([eq? operation 'mode] current-mode)
        
        ([eq? operation 'toggle-curfew]
         (set! curfew-activated (not curfew-activated)))

        ([eq? operation 'curfew-activated?]
         curfew-activated)

        ([eq? operation 'curfew-active?]
         (curfew-active? model curfew-activated))
        
        ([eq? operation 'enter]
         (handle-can-do operation id-cat))
        
        ([eq? operation 'leave]
          (handle-can-do operation id-cat))

        (else error (string-append "Unsupported operation " operation))))

  handle)

(define (can-do? model operation id-cat chips-registry selected-mode curfew-activated)
  (if (registered? id-cat chips-registry)
      (can-registered-cat-do? model operation selected-mode curfew-activated)
      (can-unregistered-cat-do? model operation)))

(define (boolean-response->text-response boolean-response)
    (if [eq? #t boolean-response]
        "OK"
        "PERMISSION DENIED"))
 