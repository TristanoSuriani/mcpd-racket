#lang racket

;; Now unit tests!

(require rackunit)
(require "../src/user-interface.rkt")
(require "../src/parser.rkt")

(define test-model (parse-mcpd-file "../src/rules.mcpd"))

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