#lang racket


(provide make-user-interface)

(require "model.rkt")
(require "chips-registry.rkt")
(require "interpreter.rkt")

;; Here we handle the owners's (register, unregister, reset, switch-mode) feline's requests (which are: enter, leave, and meow)

(define (make-user-interface model)
  (define chips-registry '())
  (define current-mode "default")
  (define curfew-activated #f)
  (define modes-names (map (λ (mode) (mode-name mode)) (model-modes model)))

  (define (handle-can-do operation id-cat)
    (boolean-response->text-response (can-do? model operation id-cat chips-registry current-mode curfew-activated)))

  (define (handle operation . other-args)
    (let
        ([id-cat
          (if (pair? other-args)
              (first other-args)
              '())])
      (cond
        ([eq? operation 'register]
         (set! chips-registry (register-chip id-cat chips-registry)))
        
        ([eq? operation 'unregister]
         (set! chips-registry (unregister-chip id-cat chips-registry)))
        
        ([eq? operation 'reset]
         (set! chips-registry (factory-reset)))
        
        ([eq? operation 'registered?]
         (registered? id-cat chips-registry))
        
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

        (else error (string-append "Unsupported operation " operation)))))

  handle)

(define (can-do? model operation id-cat chips-registry selected-mode curfew-activated)
  (if (registered? id-cat chips-registry)
      (can-registered-cat-do? model operation selected-mode curfew-activated)
      (can-unregistered-cat-do? model operation)))

(define (boolean-response->text-response boolean-response)
    (if [eq? #t boolean-response]
        "OK"
        "PERMISSION DENIED"))

;; Now unit tests!

(require rackunit)
(require "parser.rkt")

(define test-model (parse-mcpd-file "rules.mcpd"))

(test-case "An unregistered cat can leave but not enter"
           (define registry '())
           (define ask (make-user-interface test-model))
           (check-equal? (ask 'registered? 1) #f)
           (check-equal? (ask 'leave 1) "OK")
           (check-equal? (ask 'enter 1) "PERMISSION DENIED"))

(test-case "A registered cat in default mode can enter but not leave"
           (define registry '())
           (define ask (make-user-interface test-model))
           (ask 'register 1)
           (check-equal? (ask 'registered? 1) #t)
           (check-equal? (ask 'leave 1) "PERMISSION DENIED")
           (check-equal? (ask 'enter 1) "OK"))

(test-case "A registered cat in 'only enter' mode can enter but not leave"
           (define registry '())
           (define ask (make-user-interface test-model))
           (check-equal? (ask 'switch-mode) "only enter")
           (ask 'register 1)
           (check-equal? (ask 'registered? 1) #t)
           (check-equal? (ask 'leave 1) "PERMISSION DENIED")
           (check-equal? (ask 'enter 1) "OK"))

(test-case "A registered cat in 'only leave' mode can leave but not enter"
           (define registry '())
           (define ask (make-user-interface test-model))
           (check-equal? (ask 'switch-mode) "only enter")
           (check-equal? (ask 'switch-mode) "only leave")
           (ask 'register 1)
           (check-equal? (ask 'registered? 1) #t)
           (check-equal? (ask 'leave 1) "OK")
           (check-equal? (ask 'enter 1) "PERMISSION DENIED"))

(test-case "A registered cat in 'fully closed' mode cannot leave and cannot enter"
           (define registry '())
           (define ask (make-user-interface test-model))
           (check-equal? (ask 'switch-mode) "only enter")
           (check-equal? (ask 'switch-mode) "only leave")
           (check-equal? (ask 'switch-mode) "fully closed")
           (ask 'register 1)
           (check-equal? (ask 'registered? 1) #t)
           (check-equal? (ask 'leave 1) "PERMISSION DENIED")
           (check-equal? (ask 'enter 1) "PERMISSION DENIED"))

(test-case "A registered cat in 'fully open' mode can leave and enter"
           (define registry '())
           (define ask (make-user-interface test-model))
           (check-equal? (ask 'switch-mode) "only enter")
           (check-equal? (ask 'switch-mode) "only leave")
           (check-equal? (ask 'switch-mode) "fully closed")
           (check-equal? (ask 'switch-mode) "default")
           (check-equal? (ask 'switch-mode) "fully open")
           (ask 'register 1)
           (check-equal? (ask 'registered? 1) #t)
           (check-equal? (ask 'leave 1) "OK")
           (check-equal? (ask 'enter 1) "OK"))

(test-case "The reset function eliminates all entries in the chips registry"
           (define registry '())
           (define ask (make-user-interface test-model))
           (ask 'register 1)
           (check-equal? (ask 'registered? 1) #t)
           (ask 'register 2)
           (check-equal? (ask 'registered? 1) #t)
           (check-equal? (ask 'registered? 2) #t)
           (ask 'register 3)
           (check-equal? (ask 'registered? 1) #t)
           (check-equal? (ask 'registered? 2) #t)
           (check-equal? (ask 'registered? 3) #t)
           (ask 'reset)
           (check-equal? (ask 'registered? 1) #f)
           (check-equal? (ask 'registered? 2) #f)
           (check-equal? (ask 'registered? 3) #f))

(test-case "The curfew doesn't overwrite the rules for unregistered cats"
           (define registry '())
           (define ask (make-user-interface test-model))
           (ask 'toggle-curfew)
           (check-equal? (ask 'enter 1) "PERMISSION DENIED")
           (check-equal? (ask 'leave 1) "OK"))

(test-case "The curfew overwrites the rules for unregistered cats when active"
           (define registry '())
           (define ask (make-user-interface test-model))
           (ask 'register 1)
           (check-equal? (ask 'curfew-activated?) #f)
           (ask 'toggle-curfew)
           (check-equal? (ask 'curfew-activated?) #t)
           (check-equal? (ask 'switch-mode) "only enter")
           (check-equal? (ask 'switch-mode) "only leave")
           (check-equal? (ask 'switch-mode) "fully closed")
           (check-equal? (ask 'enter 1) "OK")
           (check-equal? (ask 'leave 1) "PERMISSION DENIED"))