#lang racket

;;; Holds a mcpd session and provides an interactive prompt.

(provide

 ;; [Function] Starts a new interactive prompt with a Microchip Pet Door session.
 ;; model -> function
 new-mcpd-session)

(require "model.rkt")
(require "parser.rkt")
(require "registry.rkt")
(require racket/date)
(require racket/pretty)

; ------------------------
; implementation

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

  ; Allows to activate/deactivate the special curfew mode.
  ; void -> void
  (define (toggle-curfew!)
    (set! curfew-activated (not curfew-activated)))

  ; Creates the interactive prompt.
  ; void -> function
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
                             'nothing))
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

               ((eq? 'nothing command)
                (display ""))

               ((eq? 'registered command)
                (cond
                  ((null? cat-id)
                   (print-missing-cat-id))
                  (else
                   (displayln (boolean-response->yes-no (chip-registered? cat-id registry))))))
                
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
                (begin
                  (set! registry (new-registry))
                  (print-done)))

               ((eq? 'debug command) (print-state))

               ((eq? 'help command)
                (print-help))
                
               (else
                (displayln (string-append "Unknown command " (symbol->string command) "."))))))))))

  ; Allows to select the next mode.
  ; void -> void
  (define (switch-mode!)
    (set! modes-names (append (cdr modes-names) (list (car modes-names))))
    (set! active-mode (first modes-names))
    (set! can-registered-cat-enter (can-enter-registered? active-mode model))
    (set! can-registered-cat-leave (can-leave-registered? active-mode model)))

  ; Allows to modify the registry if a cat-id has been provided, returns an error message otherwise.
  ; function, integer -> void
  (define (modify-registry! fn cat-id)
    (cond
      ((null? cat-id)
       (print-missing-cat-id))
      (else
       (begin
         (set! registry (fn cat-id registry))
         (print-done)))))      

  ; Utility function to create an infinite loop that can only be broken by the command quit.
  ; function -> void
  (define (infinite-loop fn)
    (let
        ((result (fn)))
      (cond
        ((eq? result 'quit) "Bye.")
        (else (infinite-loop fn)))))

  ; Prints the help message with a list of the allowed operations.
  ; void -> void
  (define (print-help)
    (let
        ((help '(("curfew" "Checks if the curfew is active" "curfew")
                 ("enter" "Asks if the cat can enter" "enter [id]")
                 ("leave" "Asks if the cat can leave" "leave [id]")
                 ("help" "Prints the list of available commands" "help")
                 ("mode" "Displays the active mode" "mode")
                 ("register" "Adds a cat to the registry" "register [id]*")
                 ("registered" "Checks if a cat is registered" "registered [id]*")
                 ("reset-registry" "Resets the registry" "reset-registry")
                 ("switch-mode" "Switches to the next mode" "switch-mode")
                 ("toggle-curfew" "Activates/deactivates the special curfew mode" "toggle-curfew")
                 ("unregister" "Removes a cat from the registry" "unregister [id]*"))))

      (map (λ (var)
             (let
                 ((command (first var))
                  (description (second var))
                  (usage (third var)))
               (displayln (string-append "- " command ": " description ".\n\tUsage:> " usage "\n"))))
           help)))

  ; Utility debugging function to print and inspect that state of a mcpd session.
  ; void -> void
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
  
  (prompt))

; Utility function to know if the curfew mode is active.
; model, boolean -> boolean
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

; Utility function to translate a boolean response to a user friendly message when checking that an operation is allowed.
; boolean -> string
(define (boolean-response->text-response boolean-response)
  (if [eq? #t boolean-response]
      "Ok."
      "Permisison denied."))

; Utility function to translate a boolean response to a user friendly message when checking the result of a query.
; boolean -> string
(define (boolean-response->yes-no boolean-response)
  (if (eq? boolean-response #t)
      "Yes."
      "No."))

; Utility function to print "Done."
; void -> void
(define (print-done)
  (displayln "Done."))

; Utility function to inform the user that the cat id is missing.
; void -> void
(define (print-missing-cat-id)
  (displayln (string-append "Error: missing cat id")))
