#lang knit typed/racket

;; gui for chart
(require typed/racket/gui)

;#|
;; small symbols
(define myfont (make-object font% 12 "StitchMastery Dash" 'symbol))
(define chart-symbol 16) ; pixels per symbol
;|#
#|
;; large symbols
(define myfont (make-object font% 16 "StitchMastery Dash" 'symbol))
(define chart-symbol 22) ; pixels per symbol
|#

(define mypen (make-pen #:color "Black" #:width 1))

(define chart-width 21) ; number of stitches
(define chart-height 21) ; number of rows
(define chart-gap (* 2 chart-symbol)) ; pixel gap around chart
(define frame (new frame%
                   [label "Pattern 1"]
                   [width  (+ (* chart-width  chart-symbol) (* 2 chart-gap) 14)]
                   [height (+ (* chart-height chart-symbol) (* 2 chart-gap) 37)]))
(new canvas% [parent frame]
             [paint-callback
              (lambda (canvas dc)
                (send dc set-pen mypen)
                (send dc set-scale 1 1)
                (send dc set-text-mode 'solid)
                (send dc set-text-background "white smoke")
                (send dc set-text-foreground "black")
                (send dc set-font myfont)
                (send dc draw-text "kpyTkpyTkpyTkpyTkpyT"
                      (+ 1 chart-gap)
                      (+ 1 chart-gap (* 0 chart-symbol)))
                (send dc draw-text "AJKLMNOPQRSTUVWXYZ"
                      (+ 1 chart-gap)
                      (+ 1 chart-gap (* 1 chart-symbol)))
                (send dc draw-text "ijklmnopqrstuvwxyz"
                      (+ 1 chart-gap)
                      (+ 1 chart-gap (* 2 chart-symbol)))
                (send dc draw-text "!\"#$%&'()*+,-./0"
                      (+ 1 chart-gap)
                      (+ 1 chart-gap (* 3 chart-symbol)))
                (send dc draw-text ":;<=>?@[\\]^_{}~"
                      (+ 1 chart-gap)
                      (+ 1 chart-gap (* 4 chart-symbol)))
                (send dc draw-text "oBoCoDoEoFoGoHoI" ; diacritics
                      (+ 1 chart-gap)
                      (+ 1 chart-gap (* 5 chart-symbol)))
                (send dc draw-text "*`*a*b*c*d*e*f*g*h*\xfc" ; diacritics
                      (+ 1 chart-gap)
                      (+ 1 chart-gap (* 6 chart-symbol)))
                (send dc draw-text "0102030405060708090\xfd" ; diacritics
                      (+ 1 chart-gap)
                      (+ 1 chart-gap (* 7 chart-symbol)))
                (send dc draw-text "k|" ; | diacritic
                      (+ 1 chart-gap)
                      (+ 1 chart-gap (* 8 chart-symbol)))
                (send dc draw-text "\xbc\xbf"
                      (+ 1 chart-gap)
                      (+ 1 chart-gap (* 9 chart-symbol)))
                (send dc draw-text "ijklmnopqrstuvwxyz"
                      (+ 1 chart-gap)
                      (+ 1 chart-gap (* (sub1 chart-height) chart-symbol)))
                #|
                ;; chart frame
                (send dc draw-rectangle
                      chart-gap chart-gap
                      (+ (* chart-width  chart-symbol) 1)
                      (+ (* chart-height chart-symbol) 1))
                |#
                ;; horizontal lines
                (for ([i (in-range (add1 chart-height))])
                  (send dc draw-line
                        chart-gap
                        (+ chart-gap (* i chart-symbol))
                        {+ chart-gap (* chart-width chart-symbol)}
                        (+ chart-gap (* i chart-symbol))))
                ;; vertical lines
                (for ([i (in-range (add1 chart-width))])
                  (send dc draw-line
                        (+ chart-gap (* i chart-symbol))
                        chart-gap
                        (+ chart-gap (* i chart-symbol))
                        (+ chart-gap (* chart-height chart-symbol))))
                )])
(send frame show #t)