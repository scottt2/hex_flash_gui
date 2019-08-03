#lang racket

(require racket/gui/base)

(define game%
  (class object%
    (field (score 0))
    (field (time (current-seconds)))
    (field (times (list)))
    (field (value (random 0 255)))
    (define/public (check-guess guess)
      (let ([g (if (string? guess) (string->number guess) guess)])
        (if (boolean? g) g (eq? g value))))
    (define/public (get-average-time)
      (ceiling (/ (foldl + 0 times) (length times))))
    (define/public (mark-correct)
      (set! score (+ score 1))
      (set! times (append times (list (- (current-seconds) time))))
      (set! value (random 0 255)))
    (define/public (reset-timer)
      (set! time (current-seconds)))
    (super-new)))
(define game (new game%))

(define (handle-success i)
  (send game mark-correct)
  (message-box
    ""
    (format
      "You are correct after ~a seconds.~%Way to go, champ! Your mom is proud."
      (last (dynamic-get-field 'times game)))
    frame)
  (send num set-label (format "~X" (dynamic-get-field 'value game)))
  (send score set-label (format "~a" (dynamic-get-field 'score game)))
  (send avg set-label (format "~a" (send game get-average-time)))
  (send game reset-timer)
  (send (send i get-editor) erase))

(define (handle-guess i e)
  (when (eq? (send e get-event-type) 'text-field-enter)
    (if (send game check-guess (send i get-value)) (handle-success i) null)))

(define frame (new frame% [label "HexFlash v2.1"]
                          [height 200]
                          [width 400]
                          [style '(no-resize-border)]))
(send frame center 'both)

(define menu-bar (new menu-bar% [parent frame]))
(define file-menu
  (new menu%
    [label "&File"]
    [parent menu-bar]))
(define quit-item (new menu-item%
  [label "Q&uit"]
  [parent file-menu]
  [callback
    (lambda (m event)
      (exit '()))]))
 
(define panel (new horizontal-panel% [parent frame]))

;; Left Panel
(define left (new vertical-panel% [parent panel]
                                  [min-width 200]))

(define num (new message% [parent left]
                          [auto-resize #t]
                          [font (make-object font% 100 'default)]
                          [label (format "~X" (dynamic-get-field 'value game))]))

(define guess (new text-field% [parent left]
                               [label "What is the above value as a decimal?"]
                               [horiz-margin 42]
                               [style '(single vertical-label)]
                               [callback handle-guess]))

;; Right Panel
(define right (new vertical-panel% [parent panel]))

(define score (new message% [parent right]
                            [auto-resize #t]
                            [font (make-object font% 42 'modern)]
                            [label (format "~a" (dynamic-get-field 'score game))]))

(define score-label (new message% [parent right]
                                  [label "points"]))

(define avg (new message% [parent right]
                          [auto-resize #t]
                          [font (make-object font% 45 'modern)]
                          [label "∞"]))

(define avg-label (new message% [parent right]
                                [label "avg. sec"]))

(send frame show #t)
