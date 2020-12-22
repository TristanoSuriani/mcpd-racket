#lang racket

(provide
 can-unregistered-cat-do?
 can-registered-cat-do?
 curfew-active?)

(require "model.rkt")
(require racket/date)

; Here we define how the system responds to a cat whose chip hasn't been registered
(define (can-unregistered-cat-do? model operation)
  (let*
      ([rules (model-rules model)]
       [enter-rule (first (filter (λ (rule) (string=? "enter" (rule-name rule))) rules))]
       [leave-rule (first (filter (λ (rule) (string=? "leave" (rule-name rule))) rules))]
       [enter-rule-value (eq? 'always (rule-condition enter-rule))]
       [leave-rule-value (eq? 'always (rule-condition leave-rule))])

    (cond
      ([eq? 'enter operation] enter-rule-value)
      ([eq? 'leave operation] leave-rule-value)
      (else #f))))

; Here we define how the system responds to a cat whose chip has been registered
(define (can-registered-cat-do? model operation selected-mode curfew-activated)  
  (let*
      ([mode (model-mode-from-mode-name model selected-mode)]
       [curfew (model-curfew model)]
       [can-enter-curfew (curfew-can-enter curfew)]
       [can-leave-curfew (curfew-can-leave curfew)]
       [can-enter (mode-can-enter mode)]
       [can-leave (mode-can-leave mode)])

    (cond
      ([eq? 'enter operation]
       (if [curfew-active? model curfew-activated]
           can-enter-curfew
           can-enter))
       
      ([eq? 'leave operation]
       (if [curfew-active? model curfew-activated]
           can-leave-curfew
           can-leave))
      
      [else #f])))

; Utility function to determine if the curfew is active
(define (curfew-active? model curfew-activated)
  (let*
      ([curfew-definition (model-curfew model)]
       [starts (curfew-starts curfew-definition)]
       [ends (curfew-ends curfew-definition)]
       [ends (if (> starts ends) (+ 24 ends) ends)]
       [hour (date-hour (current-date))])

    (and
     curfew-activated
     (>= hour starts)
     (<= hour ends))))

