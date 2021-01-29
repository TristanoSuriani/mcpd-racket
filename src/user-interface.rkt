#lang racket

;;; TODO add documentation
(provide new-mcpd-session)

(require "model.rkt")
(require "parser.rkt")
(require "chips-registry.rkt")
(require racket/date)
(require racket/pretty)

(define (new-mcpd-session model)
  (define registry (new-registry))
  (define modes-names (get-modes-names model))
  (define active-mode (first modes-names))
  (define curfew-activated #f)
  (define can-unregistered-cat-enter (can-enter-unregistered? model))
  (define can-unregistered-cat-leave (can-leave-unregistered? model))
  (define can-registered-cat-enter (can-enter-registered? active-mode model))
  (define can-registered-cat-leave (can-leave-registered? active-mode model))
  (define can-cat-enter-during-curfew (can-enter-during-curfew? model))
  (define can-cat-leave-during-curfew (can-leave-during-curfew? model))

  (define (toggle-curfew!)
    (set! curfew-activated (not curfew-activated)))

  (define (prompt)
    (begin
      (displayln "Microchip Pet Door v0.0.1-pre-alpha by Tristano Suriani")
      (infinite-loop
       (λ ()
         (begin
           (display "@Microchip-Pet-Door> ")
           (let*
               ((text-input (read-line))
                (input (if (eof-object? text-input)
                           '()
                           (string-split (string-trim text-input) " ")))
                (command (if (>= (length input) 1)
                             (string->symbol (first input))
                             "nothing"))
                (cat-id (if (>= (length input) 2)
                            (second input)
                            '())))
            
             (cond
               ((eq? 'quit command) command)

               ((eq? 'enter command)
                (cond
                  ((curfew? model curfew-activated)
                   (displayln (boolean-response->text-response can-cat-enter-during-curfew)))
                  ((and
                    (not (null? cat-id))
                    (chip-registered? cat-id registry))
                   (displayln (boolean-response->text-response can-registered-cat-enter)))
                  (else (displayln (boolean-response->text-response can-unregistered-cat-enter)))))

               ((eq? 'leave command)
                (cond
                  ((curfew? model curfew-activated)
                   (displayln (boolean-response->text-response can-cat-leave-during-curfew)))
                  ((and
                    (not (null? cat-id))
                    (chip-registered? cat-id registry))
                   (displayln (boolean-response->text-response can-registered-cat-leave)))
                  (else (displayln (boolean-response->text-response can-unregistered-cat-leave)))))

               ((eq? 'register command)
                (modify-registry! chip-register cat-id))

               ((eq? 'unregister command)
                (modify-registry! chip-unregister cat-id))

               ((eq? 'registered command)
                (displayln (boolean-response->yes-no (chip-registered? cat-id registry))))
                
               ((eq? 'mode command) (displayln active-mode))
                
               ((eq? 'switch-mode command)
                (begin
                  (switch-mode!)
                  (displayln active-mode)))

               ((eq? 'toggle-curfew command)
                (begin
                  (toggle-curfew!)
                  (print-done)))

               ((eq? 'curfew command)
                 (displayln (boolean-response->yes-no (curfew? model curfew-activated))))

               ((eq? 'reset-registry command)
                (set! registry (new-registry)))

               ((eq? 'debug command) (print-state))
                
               (else
                (displayln (string-append "Unknown command " (symbol->string command) "."))))))))))

  (define (switch-mode!)
    (set! modes-names (append (cdr modes-names) (list (car modes-names))))
    (set! active-mode (first modes-names))
    (set! can-registered-cat-enter (can-enter-registered? active-mode model))
    (set! can-registered-cat-leave (can-leave-registered? active-mode model)))
  
  (define (modify-registry! fn cat-id)
    (cond
      ((null? cat-id)
       (print-missing-cat-id))
      (else
       (begin
         (set! registry (fn cat-id registry))
         (print-done)))))      
  
  (define (infinite-loop fn)
    (let
        ((result (fn)))
      (cond
        ((eq? result 'quit) "Bye.")
        (else (infinite-loop fn)))))

  (define (print-state)
    (map (λ (var) (pretty-print var))
         (list "model"
               model
               "registry"
               registry
               "modes-names"
               modes-names
               "active-mode"
               active-mode
               "curfew-activated"
               curfew-activated
               "can-unregistered-cat-enter"
               can-unregistered-cat-enter
               "can-unregistered-cat-leave"
               can-unregistered-cat-leave
               "can-registered-cat-enter"
               can-registered-cat-enter
               "can-registered-cat-leave"
               can-registered-cat-leave
               "can-cat-enter-during-curfew"
               can-cat-enter-during-curfew
               "can-cat-leave-during-curfew"
               can-cat-leave-during-curfew)))
  
  prompt)

(define (curfew? model curfew-activated)
  (let*
      ([starts (model-curfew-starts model)]
       [ends (model-curfew-ends model)]
       [ends (if (> starts ends) (+ 24 ends) ends)]
       [hour (date-hour (current-date))])

    (and
     curfew-activated
     (>= hour starts)
     (<= hour ends))))


(define (boolean-response->text-response boolean-response)
  (if [eq? #t boolean-response]
      "Ok."
      "Permisison denied."))

(define (boolean-response->yes-no boolean-response)
  (if (eq? boolean-response #t)
      "Yes."
      "No."))
  
(define (print-done)
  (displayln "Done."))

(define (print-missing-cat-id)
  (displayln (string-append "Error: missing cat id")))
