#lang racket

;;; Unit tests for the user-interface module

; ------------------------
; implementation

(require rackunit)
(require "../src/user-interface.rkt")
(require "../src/parser.rkt")
(require "../src/chips-registry.rkt")

(define test-model (parse-mcpd-file "../src/rules.mcpd"))

(test-case "An unregistered cat can leave but not enter"
           (define registry (new-registry))
           (define ui (new-user-interface test-model))
           
           (check-equal?
            (ui-request(ui) registered? with chip 1)
            #f)
           
           (check-equal?
            (ui-request(ui) leave with chip 1)
            "OK")
           
           (check-equal?
            (ui-request(ui) enter with chip 1)
            "PERMISSION DENIED"))

(test-case "A registered cat in default mode can enter but not leave"
           (define registry (new-registry))
           (define ui (new-user-interface test-model))
           
           (ui-request(ui) register with chip 1)
           
           (check-equal?
            (ui-request(ui) registered? with chip 1)
            #t)
           
           (check-equal?
            (ui-request(ui) leave with chip 1)
            "PERMISSION DENIED")
           
           (check-equal?
            (ui-request(ui) enter with chip 1)
            "OK"))

(test-case "A registered cat in 'only enter' mode can enter but not leave"
           (define registry (new-registry))
           (define ui (new-user-interface test-model))
           
           (check-equal?
            (ui-request(ui) switch-mode)
            "only enter")
           
           (ui-request(ui) register with chip 1)
           
           (check-equal?
            (ui-request(ui) registered? with chip 1)
            #t)
           
           (check-equal?
            (ui-request(ui) leave with chip 1)
            "PERMISSION DENIED")
           
           (check-equal?
            (ui-request(ui) enter with chip 1)
            "OK"))

(test-case "A registered cat in 'only leave' mode can leave but not enter"
           (define registry (new-registry))
           (define ui (new-user-interface test-model))
           
           (check-equal?
            (ui-request(ui) switch-mode)
            "only enter")
           
           (check-equal?
            (ui-request(ui) switch-mode)
            "only leave")
           
           (ui-request(ui) register with chip 1)
           
           (check-equal?
            (ui-request(ui) registered? with chip 1)
            #t)
           
           (check-equal?
            (ui-request(ui) leave with chip 1)
            "OK")
           
           (check-equal?
            (ui-request(ui) enter with chip 1)
            "PERMISSION DENIED"))

(test-case "A registered cat in 'fully closed' mode cannot leave and cannot enter"
           (define registry (new-registry))
           (define ui (new-user-interface test-model))
           
           (check-equal?
            (ui-request(ui) switch-mode)
            "only enter")
           
           (check-equal?
            (ui-request(ui) switch-mode)
            "only leave")
           
           (check-equal?
            (ui-request(ui) switch-mode)
            "fully closed")
           
           (ui-request(ui) register with chip 1)
           
           (check-equal?
            (ui-request(ui) registered? with chip 1)
            #t)
           
           (check-equal?
            (ui-request(ui) leave with chip 1)
            "PERMISSION DENIED")
           
           (check-equal?
            (ui-request(ui) enter with chip 1)
            "PERMISSION DENIED"))

(test-case "A registered cat in 'fully open' mode can leave and enter"
           (define registry (new-registry))
           (define ui (new-user-interface test-model))
           
           (check-equal?
            (ui-request(ui) switch-mode)
            "only enter")
           
           (check-equal?
            (ui-request(ui) switch-mode)
            "only leave")
           
           (check-equal?
            (ui-request(ui) switch-mode)
            "fully closed")

           (check-equal?
            (ui-request(ui) switch-mode)
            "default")

           (check-equal?
            (ui-request(ui) switch-mode)
            "fully open")
           
           (ui-request(ui) register with chip 1)
           
           (check-equal?
            (ui-request(ui) registered? with chip 1)
            #t)
           
           (check-equal?
            (ui-request(ui) leave with chip 1)
            "OK")
           
           (check-equal?
            (ui-request(ui) enter with chip 1)
            "OK"))


(test-case "The reset function eliminates all entries in the chips registry"
           (define registry (new-registry))
           (define ui (new-user-interface test-model))

           (ui-request(ui) register with chip 1)
           (ui-request(ui) register with chip 2)
           (ui-request(ui) register with chip 3)

           (check-equal?
            (ui-request(ui) registered?  with chip 1)
            #t)

           (check-equal?
            (ui-request(ui) registered?  with chip 2)
            #t)

           (check-equal?
            (ui-request(ui) registered?  with chip 3)
            #t)
           
           (ui-request(ui) reset)
           
           (check-equal?
            (ui-request(ui) registered?  with chip 1)
            #f)

           (check-equal?
            (ui-request(ui) registered?  with chip 2)
            #f)

           (check-equal?
            (ui-request(ui) registered?  with chip 3)
            #f))

(test-case "The curfew doesn't overwrite the rules for unregistered cats"
           (define registry (new-registry))
           (define ui (new-user-interface test-model))
           
           (ui-request(ui) toggle-curfew)
           
           (check-equal?
            (ui-request(ui) enter with chip 1)
            "PERMISSION DENIED")
           
           (check-equal?
            (ui-request(ui) leave with chip 1)
            "OK"))

(test-case "The curfew overwrites the rules for unregistered cats when active"
           (define registry (new-registry))
           (define ui (new-user-interface test-model))
           
           (ui-request(ui) register with chip 1)
           
           (check-equal?
            (ui-request(ui) curfew-activated?)
            #f)
           
           (ui-request(ui) toggle-curfew)
           
           (check-equal?
            (ui-request(ui) curfew-activated?)
            #t)
           
           (check-equal?
            (ui-request(ui) switch-mode)
            "only enter")
           
           (check-equal?
            (ui-request(ui) switch-mode)
            "only leave")
           
           (check-equal?
            (ui-request(ui) enter with chip 1)
            "OK")
           
           (check-equal?
            (ui-request(ui) leave with chip 1)
            "PERMISSION DENIED"))
