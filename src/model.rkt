#lang racket

;;; Defines the model for the MCPD domain specific language.

(provide
 ;; Returns an empty model
 empty-model

 ;; Adds a mode to a model
add-mode

;; Retrieves a list of the modes' names
get-modes-names

;; Retrieves a mode from its name
get-mode-by-name

;; Adds the curfew section to a model
add-curfew-section

;; Adds the rules section to a model
add-rules-section

;; It retrieves the starting time of the curfew from a model
model-curfew-starts

;; It retrieves the ending time of the curfew from a model
model-curfew-ends

;; Answers if an unregistered cat can enter
can-enter-unregistered?

;; Answers if an unregistered cat can leave
can-leave-unregistered?

;; Answers if a registered cat can enter
can-enter-registered?

;; Answers if a registered cat can leave
can-leave-registered?

;; Answers if a cat can enter during the curfew
can-enter-during-curfew?

;; Answers if a cat can leave during the curfew
can-leave-during-curfew?

)
 
(require rackunit)

(define (empty-model)
  '((modes)
    (curfew)
    (rules)))

(define (add-mode name can-enter can-leave model)
  (insertR* 'modes `(,name (can-enter ,can-enter) (can-leave ,can-leave)) model))

(define (add-curfew-section starts ends can-enter can-leave model)
  (let
      ((curfew-body `(curfew
                      (starts ,starts)
                      (ends ,ends)
                      (can-enter ,can-enter)
                      (can-leave ,can-leave))))

    (replace* 'curfew curfew-body model)))

(define (add-rules-section can-enter can-leave model)
  (let
      ((rules-body `(rules
                     (can-enter ,can-enter)
                     (can-leave ,can-leave))))

    (replace* 'rules rules-body model)))


(define (get-modes-names model)
  (let
      ((modes (get-tagged-list 'modes model)))
    
    (map (λ (mode)
           (first mode))
         (rest modes))))

(define (get-mode-by-name name model)
  (get-tagged-list-with-tags (list 'modes name) model))
  
(define (model-curfew-starts model)
  (second (get-tagged-list-with-tags '(curfew starts) model)))

(define (model-curfew-ends model)
  (second (get-tagged-list-with-tags '(curfew starts) model)))

(define (can-enter-unregistered? model)
  (eq? (can-do? '(rules can-enter) model) 'always))

(define (can-leave-unregistered? model)
  (eq? (can-do? '(rules can-leave) model) 'always))

(define (can-enter-registered? mode model)
  (can-do? `(modes ,mode can-enter) model))

(define (can-leave-registered? mode model)
  (can-do? `(modes ,mode can-leave) model))

(define (can-enter-during-curfew? model)
  (can-do? '(curfew can-enter) model))

(define (can-leave-during-curfew? model)
  (can-do? '(curfew can-leave) model))

;; __________________________
;;
;; Utility functions
;; __________________________

(define (can-do? tags model)
   (let
      ((selected-mode (get-tagged-list-with-tags tags model)))
    (second selected-mode)))

(define (get-tagged-list tag lst)
  (cond
    ((null? lst) '())
    ((atom? (car lst))
     (cond
       ((eq? (car lst) tag) lst)
       (else (get-tagged-list tag (cdr lst)))))
     (else
      (let
          ((car-branch (get-tagged-list tag (car lst))))
      (cond
        ((null? car-branch) (get-tagged-list tag (cdr lst)))
        (else car-branch))))))

(define (get-tagged-list-with-tags tags lst)
  (cond
    ((null? lst) '())
    ((null? tags) lst)
    (else (get-tagged-list-with-tags (cdr tags) (get-tagged-list (car tags) lst)))))

(define (insertR* old new lst)
  (cond
    ((null? lst) '())
    ((atom? (car lst))
     (cond
       ((eq? (car lst) old)
        (cons old (cons new (insertR* old new (cdr lst)))))
       (else
        (cons (car lst) (insertR* old new (cdr lst))))))
    (else
     (cons (insertR* old new (car lst))
           (insertR* old new (cdr lst))))))

(define (replace* old new lst)
  (cond
    ((null? lst) '())
    ((atom? (car lst))
     (cond
       ((eq? (car lst) old)
        (cons new (replace* old new (cdr lst))))
       (else
        (cons (car lst) (replace* old new (cdr lst))))))
    (else
     (cons (replace* old new (car lst))
           (replace* old new (cdr lst))))))

(define (atom? element)
  (and
   (not (null? element))
   (not (pair? element))))


;; __________________________
;;
;; Unit tests
;; __________________________

(check-equal?
 (add-mode 'test #t #f (empty-model))
 '((modes
    (test
     (can-enter #t)
     (can-leave #f)))

   (curfew)

   (rules)))

(check-equal?
 (add-curfew-section 18 22 #t #f (empty-model))
 '((modes)
   
   ((curfew
     (starts 18)
     (ends 22)
     (can-enter #t)
     (can-leave #f)))
    
   (rules)))

(check-equal?
 (add-rules-section 'always 'when-registered (empty-model))
 '((modes)

   (curfew)

   ((rules
     (can-enter always)
     (can-leave when-registered)))))

(check-equal?
 (add-rules-section 'always 'when-registered
                    (add-curfew-section 18 22 #t #f
                                        (add-mode 'test #t #f
                                                  (add-mode 'demo #f #t (empty-model)))))
 
 '((modes
    (test
     (can-enter #t)
     (can-leave #f))
    
    (demo
     (can-enter #f)
     (can-leave #t)))
   
   ((curfew
     (starts 18)
     (ends 22)
     (can-enter #t)
     (can-leave #f)))
    
   ((rules
     (can-enter always)
     (can-leave when-registered)))))

(check-equal?
 (get-modes-names (add-curfew-section 18 22 #t #f
                                        (add-mode 'test #t #f
                                                  (add-mode 'demo #f #t (empty-model)))))
 '(test demo))

(check-equal?
 (get-mode-by-name 'demo(add-curfew-section 18 22 #t #f
                                        (add-mode 'test #t #f
                                                  (add-mode 'demo #f #t (empty-model)))))
 '(demo (can-enter #f) (can-leave #t)))

(check-equal?
 (model-curfew-starts (add-curfew-section 12 15 #t #f (empty-model)))
 12)

(check-equal?
 (model-curfew-starts (add-curfew-section 12 15 #t #f (empty-model)))
 12)

(check-equal?
 (can-enter-unregistered? (add-rules-section 'always 'when-registered (empty-model)))
 #t)

(check-equal?
 (can-leave-unregistered? (add-rules-section 'always 'when-registered (empty-model)))
 #f)

(check-equal?
 (can-enter-registered? 'test (add-mode 'test #t #f (empty-model)))
 #t)

(check-equal?
 (can-leave-registered? 'test (add-mode 'test #t #f (empty-model)))
 #f)

(check-equal?
 (can-enter-during-curfew? (add-curfew-section 18 22 #t #f (empty-model)))
 #t)

(check-equal?
 (can-leave-during-curfew? (add-curfew-section 18 22 #t #f (empty-model)))
 #f)