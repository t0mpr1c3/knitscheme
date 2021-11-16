#lang knit typed/racket
(provide (all-defined-out))

;; FIXME add checkboxes, radio buttons etc to set pattern options
;; FIXME add color to chart output
;; FIXME need documentation

;; FIXME is there a better way than this to set compile-time variables?
(define _KNITSCHEME-CHART/TEST_ #t)

(require typed/racket
         "knitscheme-base.rkt"
         typed/racket/gui
         racket/function ; needed for `curry`
         racket/vector) ; needed for `vector-map`

;; chart output functions

(: stitch-pad-decreases : (stitch -> (Vectorof stitch)))
(define (stitch-pad-decreases s)
  (let* ([st (hash-ref stitch-hash (stitch-stitchtype s))]
         [in (stitchtype-stitches-in st)]
         [out (stitchtype-stitches-out st)]
         [offset (stitchtype-offset st)])
    ;; pad decreases with no-stitches
    (if (and (> in out)
             (> out 0))
        (let ([ns (stitch #x77 #f)])
          (cond [(= -2  offset) (vector ns ns s)]
                [(= -1  offset) (vector ns s)]
                [(zero? offset) (vector ns s ns)]
                [(=  1  offset) (vector s ns)]
                [(=  2  offset) (vector s ns ns)]
                [else (error "should never get here")]))
        (vector s))))

(: leaf-pad-decreases : (Leaf -> (Vectorof stitch)))
(define (leaf-pad-decreases leaf)
  (let* ([s (leaf-stitch leaf)]
         [v (stitch-pad-decreases s)])
    (let loop ([i (leaf-count leaf)]
               [acc : (Vectorof stitch) (vector)])
      (if (zero? i)
          acc
          (loop (sub1 i) (vector-append acc v))))))

(: rowinfos-pad-decreases : ((Listof (Listof Leaf)) -> (Listof (Vectorof stitch))))
(define (rowinfos-pad-decreases flat)
  (map (λ ([r : (Listof Leaf)])
         (apply vector-append
                (map (λ ([x : Leaf]) (leaf-pad-decreases x))
                     r)))
       flat))

(: remove-zeros ((Vectorof stitch) -> (Vectorof stitch)))
(define (remove-zeros v)
  (vector-filter-not (λ ([s : stitch]) (zero? (stitch-stitchtype s))) v))

(: which-min-dist ((Listof Integer) -> (Integer -> Natural)))
(define ((which-min-dist ys) x)
  (which-min
   (map (λ ([y : Integer]) (abs (- x y)))
        ys)))

;; recursively eliminate no-stitches between increases and decreases
;; working from the middle of the row out towards both ends
(: row-squeeze : ((Vectorof stitch) (Listof Integer) (Listof Integer) (Listof Integer)
                                    -> (values (Vectorof stitch) (Listof Integer) (Listof Integer) (Listof Integer))))
(define (row-squeeze padded-r inc-is-r dec-is-r ns-is-r)
  (log-knitscheme-debug "in `rows-squeeze2` function")
  (log-knitscheme-debug (format "padded-r ~a" padded-r))
  (log-knitscheme-debug (format "inc-is-r ~a" inc-is-r))
  (log-knitscheme-debug (format "dec-is-r ~a" dec-is-r))
  (log-knitscheme-debug (format "ns-is-r ~a" ns-is-r))
  ;; recursion check
  (if (or (null? inc-is-r)
          (null? dec-is-r)
          (null? ns-is-r))
      (values
       (remove-zeros padded-r)
       inc-is-r
       dec-is-r
       ns-is-r)
      ;; greedily reduce no-stitches with shortest distance to both inc and dec
      (let* ([nearest-inc-is : (Listof Integer) (map (which-min-dist inc-is-r) ns-is-r)]
             [nearest-dec-is : (Listof Integer) (map (which-min-dist dec-is-r) ns-is-r)]
             [nearest-incs : (Listof Integer) (map (curry (ann list-ref ((Listof Integer) Integer -> Integer))
                                                          inc-is-r)
                                                   nearest-inc-is)]
             [nearest-decs : (Listof Integer) (map (curry (ann list-ref ((Listof Integer) Integer -> Integer))
                                                          dec-is-r)
                                                   nearest-dec-is)]
             [ns-i : Natural (which-min (map +
                                             (map abs (map - nearest-incs ns-is-r))
                                             (map abs (map - nearest-decs ns-is-r))))]
             [inc-i : Integer (list-ref nearest-inc-is ns-i)]
             [dec-i : Integer (list-ref nearest-dec-is ns-i)]
             [padded-i : Integer (list-ref ns-is-r ns-i)])
        ;; zero out selected no-stitch
        (vector-set! padded-r padded-i (stitch 0 #f))
        (row-squeeze
         padded-r
         (append (take inc-is-r inc-i) (list-tail inc-is-r (add1 inc-i)))
         (append (take dec-is-r dec-i) (list-tail dec-is-r (add1 dec-i)))
         (append (take ns-is-r  ns-i)  (list-tail ns-is-r  (add1 ns-i)))))))

(: rowinfos-squeeze : ((Listof (Vectorof stitch))
                       (Listof (Listof Natural))
                       (Listof (Listof Natural))
                       (Listof (Listof Natural))
                       (Listof (Vectorof stitch))
                       (Listof (Listof Integer))
                       (Listof (Listof Integer))
                       (Listof (Listof Integer))
                       -> (values (Listof (Vectorof stitch))
                                  (Listof (Listof Integer))
                                  (Listof (Listof Integer))
                                  (Listof (Listof Integer)))))
(define (rowinfos-squeeze padded inc-is dec-is ns-is squeezed-padded squeezed-inc-is squeezed-dec-is squeezed-ns-is)
  (if (null? padded)
      (values (reverse squeezed-padded)
              (reverse squeezed-inc-is)
              (reverse squeezed-dec-is)
              (reverse squeezed-ns-is))
      (let* ([padded-r (car padded)]
             [inc-is-r (car inc-is)]
             [dec-is-r (car dec-is)]
             [ns-is-r  (car ns-is)])
        (let-values ([(squeezed-once-row
                       squeezed-once-inc-is-r
                       squeezed-once-dec-is-r
                       squeezed-once-ns-is-r)
                      (row-squeeze padded-r
                                   inc-is-r
                                   dec-is-r
                                   ns-is-r)])
          ;; add virtual increases before beginning and after end
          ;; to shrink in decreases at the sides
          (let-values ([(squeezed-twice-row
                         squeezed-twice-inc-is-r
                         squeezed-twice-dec-is-r
                         squeezed-twice-ns-is-r)
                        (row-squeeze squeezed-once-row
                                     (cons -1 (cons (vector-length squeezed-once-row) squeezed-once-inc-is-r))
                                     squeezed-once-dec-is-r
                                     squeezed-once-ns-is-r)])
            (let* ([squeezed-twice-len (vector-length squeezed-twice-row)]
                   ;; prepend no-stitch if virtual increase at beginning was removed
                   [prepend-ns? (or (null? squeezed-twice-inc-is-r)
                                    (>= (apply min squeezed-twice-inc-is-r) 0))]
                   ;; append no-stitch if virtual increase at end was removed
                   [append-ns? (or (null? squeezed-twice-inc-is-r)
                                   (< (apply max squeezed-twice-inc-is-r) squeezed-twice-len))]
                   [squeezed-twice-row* (vector-append
                                         (if prepend-ns? (vector (stitch #x77 #f)) (vector))
                                         squeezed-twice-row
                                         (if append-ns? (vector (stitch #x77 #f)) (vector)))]
                   ;; filter inc-is to remove remaining virtual increases
                   ;; bump indices if prepend-ns
                   [squeezed-twice-inc-is-r* (filter (λ ([i : Integer]) (and (positive? i) (< squeezed-twice-len i)))
                                                     (if prepend-ns?
                                                         squeezed-twice-inc-is-r
                                                         (map add1 squeezed-twice-inc-is-r)))]
                   [squeezed-twice-dec-is-r* (if prepend-ns?
                                                 squeezed-twice-dec-is-r
                                                 (map add1 squeezed-twice-dec-is-r))]
                   [squeezed-twice-ns-is-r* (append (if prepend-ns? '(0) null)
                                                    (if prepend-ns?
                                                        squeezed-twice-ns-is-r
                                                        (map add1 squeezed-twice-ns-is-r))
                                                    (if append-ns? (list (sub1 (vector-length squeezed-twice-row*))) null))])
              (rowinfos-squeeze (cdr padded)
                                (cdr inc-is)
                                (cdr dec-is)
                                (cdr ns-is)
                                (cons squeezed-twice-row*
                                      squeezed-padded)
                                (cons squeezed-twice-inc-is-r*
                                      squeezed-inc-is)
                                (cons squeezed-twice-dec-is-r*
                                      squeezed-dec-is)
                                (cons squeezed-twice-ns-is-r*
                                      squeezed-ns-is))))))))

;; obtain index lists of increases, decreases, and no-stitches
;; NB all accumulated in reverse order relative to rowinfos
(: acc-idx : ((Listof (Vectorof stitch)) -> (values (Listof (Listof Natural))
                                                    (Listof (Listof Natural))
                                                    (Listof (Listof Natural)))))
(define (acc-idx padded-rs)
  ;; loop over rowinfos
  (let i-loop ([rev-padded-rs : (Listof (Vectorof stitch)) (reverse padded-rs)]
               [inc-is : (Listof (Listof Natural)) null]
               [dec-is : (Listof (Listof Natural)) null]
               [ns-is  : (Listof (Listof Natural)) null])
    (if (null? rev-padded-rs)
        (values inc-is dec-is ns-is)
        (let ([padded-rs-i (car rev-padded-rs)])
          (let-values
              ([([inc-is-j : (Listof Natural)]
                 [dec-is-j : (Listof Natural)]
                 [ns-is-j  : (Listof Natural)])
                ;; loop over bytes
                (let j-loop ([j : Integer (sub1 (vector-length padded-rs-i))]
                             [inc-is-j : (Listof Natural) null]
                             [dec-is-j : (Listof Natural) null]
                             [ns-is-j  : (Listof Natural) null])
                  (if (negative? j)
                      (values inc-is-j dec-is-j ns-is-j)
                      (let ([b (stitch-stitchtype (vector-ref padded-rs-i j))])
                        (if (= #x77 b)
                            ;; no-stitch
                            (j-loop (sub1 j)
                                    inc-is-j
                                    dec-is-j
                                    (cons j ns-is-j))
                            (let* ([st (hash-ref stitch-hash b)]
                                   [in (stitchtype-stitches-in st)]
                                   [out (stitchtype-stitches-out st)])
                              (cond
                                ;; increase
                                [(> out in) (j-loop (sub1 j)
                                                    (append (make-list (- out in) j) inc-is-j)
                                                    dec-is-j
                                                    ns-is-j)]
                                ;; decrease
                                [(> in out) (j-loop (sub1 j)
                                                    inc-is-j
                                                    (append (make-list (- in out) j) dec-is-j)
                                                    ns-is-j)]
                                ;; otherwise
                                [else       (j-loop (sub1 j)
                                                    inc-is-j
                                                    dec-is-j
                                                    ns-is-j)]))))))])
            (i-loop (cdr rev-padded-rs)
                    (cons inc-is-j inc-is)
                    (cons dec-is-j dec-is)
                    (cons ns-is-j  ns-is)))))))

(: cum-sum : ((Listof Integer) -> (Listof Integer)))
(define (cum-sum xs)
  (if (null? xs)
      null
      (let loop ([tail : (Listof Integer) (cdr xs)]
                 [acc  : (Listof Integer) (list (car xs))])
        (if (null? tail)
            (reverse acc)
            (loop (cdr tail)
                  (cons (+ (car tail)
                           (car acc))
                        acc))))))

(: rs->ws : ((Vectorof stitch) -> (Vectorof stitch)))
(define (rs->ws v)
  (vector-map (λ ([s : stitch]) (stitch (stitchtype-ws-symbol (hash-ref stitch-hash (stitch-stitchtype s)))
                                        (stitch-yarntype s)))
              v))

(: left-start? : (Pattern Positive-Index -> Boolean))
(define (left-start? p r)
  (let ([work (Pattern-work p)]
        [form (Pattern-form p)]
        [side (Pattern-side p)])
    (or (and (or (eq? 'machine work)
                 (and (eq? 'hand work)
                      (eq? 'circular form)))
             (eq? 'left side))
        (and (eq? 'hand work)
             (eq? 'flat form)
             (or (and (eq? 'left side)
                      (odd? r))
                 (and (eq? 'right side)
                      (even? r)))))))

(: right-face? : (Pattern Positive-Index -> Boolean))
(define (right-face? p r)
  (let ([work (Pattern-work p)]
        [form (Pattern-form p)]
        [face (Pattern-face p)])
    (or (and (or (eq? 'machine work)
                 (and (eq? 'hand work)
                      (eq? 'circular form)))
             (eq? 'rs face))
        ;; knitted flat
        (and (eq? 'hand work)
             (eq? 'flat form)
             (or (and (eq? 'rs face)
                      (odd? r))
                 (and (eq? 'ws face)
                      (even? r)))))))

;; create stitch matrix to print chart
;; NB * in circular knitting, rounds are all knit from the same side and on the same face
;;    * in flat knitting, each row is knit on the opposite side and face to the previous row
;;    * in machine knitting, rows are all on the same face but alternate sides
(: make-stitch-matrix : (Pattern -> (Vectorof (Vectorof stitch))))
(define (make-stitch-matrix p)
  (let* ([rowmap (Pattern-rowmap p)]
         [n-rows (Rowmap-rowcount rowmap)])
    (log-knitscheme-debug "in function `make-stitch-matrix`")
    (if (zero? n-rows)
        null
        (let* ([rownums (Rowmap-data rowmap)]
               [rowinfos (Pattern-rowinfos p)]
               [stitches (vector-map Rowinfo-stitches rowinfos)]
               [flat-stitches (map flatten-tree (vector->list stitches))]
               ;; initially pad decreases with "no stitch" symbols
               [padded-rs (rowinfos-pad-decreases flat-stitches)])
          ;; obtain index lists of increases, decreases, and no-stitches
          (let*-values ([(inc-is dec-is ns-is) (acc-idx padded-rs)]
                        ;; recursively eliminate no-stitches between increases and decreases
                        [(squeezed-rs squeezed-inc-is squeezed-dec-is squeezed-ns-is)
                         (rowinfos-squeeze padded-rs inc-is dec-is ns-is null null null null)])
            ;; next, convert rs->ws (if necessary)
            (let* ([squeezed-ws (map rs->ws squeezed-rs)]
                   ;; create vector of stitch-rows
                   [sm : (Vectorof (Vectorof stitch)) (make-vector n-rows (vector))]
                   ;; create vector of increases
                   [incs : (Vectorof (Listof Integer)) (make-vector n-rows null)]
                   ;; create vector of decreases
                   [decs : (Vectorof (Listof Integer)) (make-vector n-rows null)]
                   ;; create vector of no-stitches
                   [ns   : (Vectorof (Listof Integer)) (make-vector n-rows null)])
              ;; populate stitch-matrix (and other vectors)
              (for ([j (in-range n-rows)])
                (let* ([r (cast (add1 j) Positive-Index)]
                       [i (rowmap-find rowmap r)])
                  (if (false? i)
                      (error (format "could not find row ~a" r))
                      (let ([row-face-i (list-ref (if (right-face? p r) squeezed-rs squeezed-ws) i)])
                        (if (left-start? p r)
                            (begin
                              (vector-set! sm   j row-face-i)
                              (vector-set! incs j (list-ref squeezed-inc-is i))
                              (vector-set! decs j (list-ref squeezed-dec-is i))
                              (vector-set! ns   j (list-ref squeezed-ns-is  i)))
                            (let* ([n : Integer (vector-length row-face-i)]
                                   [rev : ((Listof Integer) -> (Listof Integer))
                                        (λ ([xs : (Listof Integer)])
                                          (reverse
                                           (map (λ ([x : Integer]) (- n x 1))
                                                xs)))])
                              (vector-set! sm   j (vector-reverse row-face-i))
                              (vector-set! incs j (rev (list-ref squeezed-inc-is i)))
                              (vector-set! decs j (rev (list-ref squeezed-dec-is i)))
                              (vector-set! ns   j (rev (list-ref squeezed-ns-is  i)))))))))
              (log-knitscheme-debug (format "squeezed-rs ~a" squeezed-rs))
              (log-knitscheme-debug (format "squeezed-ws ~a" squeezed-ws))
              (log-knitscheme-debug (format "incs ~a" incs))
              (log-knitscheme-debug (format "decs ~a" decs))
              (log-knitscheme-debug (format "ns ~a" ns))
              ;; if only 1 row, do not need to align rows
              (if (= 1 n-rows)
                  (begin
                    ;; remove all no-stitches
                    (vector-set! sm 0 (vector-filter-not (λ ([s : stitch]) (= #x77 (stitch-stitchtype s)))
                                                         (vector-ref sm 0)))
                    ;; return stitch matrix
                    (log-knitscheme-debug (format "sm ~a" sm))
                    sm)
                  (let ([n-cols (apply max (map vector-length squeezed-rs))])
                    ;; calculate vectors of stitches in/out padded with zeroes to length `n-cols`
                    (let-values ([([st-in  : (Listof (Listof Integer))]
                                   [st-out : (Listof (Listof Integer))])
                                  (let row-loop ([i : Integer (sub1 n-rows)]
                                                 [row-acc-in : (Listof (Listof Integer)) null]
                                                 [row-acc-out : (Listof (Listof Integer)) null])
                                    (if (negative? i)
                                        (values row-acc-in row-acc-out)
                                        (let stitch-loop ([v : (Listof stitch) (vector->list (vector-ref sm i))]
                                                          [st-acc-in : (Listof Integer) null]
                                                          [st-acc-out : (Listof Integer) null])
                                          (if (null? v)
                                              ;; rows accumulate in reverse order
                                              (row-loop (sub1 i)
                                                        (cons (append (reverse st-acc-in)
                                                                      (make-list (- n-cols (length st-acc-in)) 0))
                                                              row-acc-in)
                                                        (cons (append (reverse st-acc-out)
                                                                      (make-list (- n-cols (length st-acc-out)) 0))
                                                              row-acc-out))
                                              (let ([st (hash-ref stitch-hash (stitch-stitchtype (car v)))])
                                                (stitch-loop (cdr v)
                                                             ;; stitches accumulate in reverse order
                                                             (cons (stitchtype-stitches-in  st) st-acc-in)
                                                             (cons (stitchtype-stitches-out st) st-acc-out)))))))])
                      (let* ([dev : (Listof (Listof Integer))
                                  (let row-loop ([rows-in (cdr st-in)]
                                                 [rows-out st-out]
                                                 [acc : (Listof (Listof Integer)) null])
                                    (if (null? rows-in)
                                        acc
                                        (row-loop (cdr rows-in)
                                                  (cdr rows-out)
                                                  ;; list is accumulated in reverse order
                                                  (cons (map - (car rows-in) (car rows-out))
                                                        acc))))]
                             [cum-dev (map cum-sum (reverse dev))]
                             [cum-dev-minmax (map (λ ([xs : (Listof Integer)]) (cons (apply min xs)
                                                                                     (apply max xs)))
                                                  cum-dev)]
                             [offset (cum-sum (cons 0
                                                    (map (λ ([x : (Pairof Integer Integer)])
                                                           (let ([s (+ (car x) (cdr x))])
                                                             (if (< (abs s) 2)
                                                                 s
                                                                 (truncate (/ s 2)))))
                                                         cum-dev-minmax)))]
                             [min-offset (apply min offset)]
                             [prepend (map (λ ([i : Index]) (min (- (list-ref offset i) min-offset)
                                                                 (- n-cols (vector-length (vector-ref sm i)))))
                                           (range (length offset)))])
                        (log-knitscheme-debug (format "st-in ~a" st-in))
                        (log-knitscheme-debug (format "st-out ~a" st-out))
                        (log-knitscheme-debug (format "cum-dev-minmax ~a" cum-dev-minmax))
                        (log-knitscheme-debug (format "offset ~a" offset))
                        (log-knitscheme-debug (format "prepend ~a" prepend))
                        ;; align rows in stitch-matrix by prepending no-stitches
                        ;; provided that this does not extend row further than `n-cols` in width
                        (let i-loop ([i : Natural 0]
                                     [prepend~ prepend])
                          (if (= i n-rows)
                              ;; pad ends of rows with no-stitches
                              (let* ([row-lengths (vector-map vector-length sm)]
                                     [n-cols~ (apply max (vector->list row-lengths))])
                                (for ([i (in-range n-rows)])
                                  (vector-set! sm i (vector-append (vector-ref sm i)
                                                                   (make-vector (- n-cols~
                                                                                   (vector-ref row-lengths i))
                                                                                (stitch #x77 #f)))))
                                ;; return stitch matrix
                                (log-knitscheme-debug (format "sm ~a" sm))
                                sm)
                              ;; prepend no-stitches to row i
                              (let ([prepend-i (car prepend~)])
                                (vector-set! sm i (vector-append (make-vector prepend-i (stitch #x77 #f))
                                                                 (vector-ref sm i)))
                                (i-loop (add1 i) (cdr prepend~)))))))))))))))

(: draw-chart : ((Instance Frame%) Natural Natural Natural Natural Natural
                                   ((Instance DC<%>) -> Void) -> (Instance Canvas%)))
(define (draw-chart frame chart-symbol chart-height chart-width chart-vgap chart-hgap draw-thunk)
  (new canvas%
       [parent frame]
       [paint-callback
        (λ (canvas dc)
          ;; draw text first
          (draw-thunk dc)
          (send dc set-pen (make-pen #:color "black" #:width 1))
          ;; horizontal lines
          (for ([i (in-range (add1 chart-height))])
            (send dc draw-line
                  chart-hgap
                  (+ chart-vgap (* i chart-symbol))
                  {+ chart-hgap (* chart-width chart-symbol)}
                  (+ chart-vgap (* i chart-symbol))))
          ;; vertical lines
          (for ([i (in-range (add1 chart-width))])
            (send dc draw-line
                  (+ chart-hgap (* i chart-symbol))
                  chart-vgap
                  (+ chart-hgap (* i chart-symbol))
                  (+ chart-vgap (* chart-height chart-symbol)))))]))

;; chart output for Pattern
(: pattern->chart : (Pattern -> Void))
(define (pattern->chart p)
  (let* ([sm (make-stitch-matrix p)]
         [rowinfos (Pattern-rowinfos p)]
         [rowmap (Pattern-rowmap p)]
         [n-rows (Rowmap-rowcount rowmap)]
         ;; small symbols
         [knitfont (make-object font% 12 "StitchMastery Dash" 'symbol)]
         [rowfont (make-object font% 10 'modern)]
         [chart-symbol 16] ; pixels per symbol
         [chart-width (cast (apply max (vector->list (vector-map vector-length sm))) Natural)] ; number of stitches in chart
         [chart-height n-rows] ; number of rows in chart
         [chart-vgap (exact-floor (* 1.5 chart-symbol))] ; pixel gap around top/bottom of chart
         [chart-hgap (exact-floor (* 3 chart-symbol))] ; pixel gap around sides of chart
         [frame (new frame%
                     [label (Pattern-name p)]
                     [width  (+ (* chart-width  chart-symbol) (* 2 chart-hgap) 14)]
                     [height (+ (* chart-height chart-symbol) (* 2 chart-vgap) 37)])]
         [white (make-object color% #xFF #xFF #xFF)]
         [black (make-object color% #x00 #x00 #x00)]
         [canvas
          (draw-chart
           frame chart-symbol chart-height chart-width chart-vgap chart-hgap
           (λ ([dc : (Instance DC<%>)])
             (send dc set-scale 1 1)
             (send dc set-text-mode 'solid)
             (for ([y (in-range n-rows)])
               (let* ([r (cast (add1 y) Positive-Index)]
                      [sm-r (vector-ref sm y)]
                      [row-str (~a r)])
                 ;; draw row numbers
                 (send dc set-text-background white)
                 (send dc set-text-foreground black)
                 (send dc set-font rowfont)
                 (send dc draw-text
                       row-str
                       (+ 0 chart-hgap
                          (* chart-symbol
                             (if (left-start? p r)
                                 (- -0.5 (* 0.5 (string-length row-str)))
                                 (+ chart-width 0.5))))
                       (+ 0 chart-vgap
                          (* chart-symbol
                             (- n-rows r))))
                 ;; draw stitch symbols
                 (send dc set-font knitfont)
                 (let* ([yrns : (Immutable-Vectorof (Option yarntype)) (Pattern-yarns p)]
                        [sym : (Vectorof Byte) (vector-map (λ ([s : stitch]) (stitch-stitchtype s))
                                                           sm-r)]
                        [color : (Vectorof (Option (Instance Color%)))
                               (vector-map
                                (λ ([s : stitch])
                                  (let ([y (stitch-yarntype s)])
                                    (if (false? y)
                                        #f
                                        (let ([yrn (vector-ref yrns y)])
                                          (if (false? yrn)
                                              #f
                                              (let ([c (send the-color-database find-color (yarntype-color yrn))])
                                                (if (false? c)
                                                    ;; default to white if yarn color not found
                                                    white
                                                    c)))))))
                                sm-r)])
                   (for ([x (in-range (vector-length sm-r))])
                     (let ([c (vector-ref color x)])
                       (if (false? c)
                           (begin
                             ;; gray on gray for no-stitches
                             (send dc set-text-background "white smoke")
                             (send dc set-text-foreground "gray"))
                           (begin
                             (send dc set-text-background c)
                             (send dc set-text-foreground black)))
                       (send dc draw-text
                             (bytes->string/latin-1 (bytes (vector-ref sym x)))
                             (+ 1 chart-hgap (* x chart-symbol))
                             (+ 1 chart-vgap (* (- n-rows r) chart-symbol)))))
                   (void))))))])
    (send frame show #t)))

;; tests

(require typed/rackunit
         adjutor) ; for `values->list`

(when _KNITSCHEME-CHART/TEST_

  (log-knitscheme-info "start knitscheme-chart tests")

  ;; tests of `row-squeeze` function
  (log-knitscheme-debug "start tests of `yarns->text` function")

  (: bytes->stitch-row : (Bytes -> (Vectorof stitch)))
  (define (bytes->stitch-row b)
    (list->vector (map (λ ([x : Byte]) (stitch x (if (= #x77 x) #f 0))) (bytes->list b))))

  (check-equal?
   (car (values->list
         (row-squeeze (bytes->stitch-row #"=XX.*.X-..-X.*.XX=")
                      '(13 13 4 4)
                      '(17 17 10 7 0 0)
                      '(16 15 11 6 2 1))))
   (bytes->stitch-row #"=X.*.-..-.*.X="))

  (check-equal?
   (car (values->list
         (row-squeeze (bytes->stitch-row #"X=X+++=XX")
                      '(5 4 3)
                      '(6 6 1 1)
                      '(8 7 2 0))))
   (bytes->stitch-row #"=+++=X"))

  (check-equal?
   (car (values->list
         (row-squeeze (bytes->stitch-row #"XX=+++X=X")
                      '(5 4 3)
                      '(7 7 2 2)
                      '(8 6 0 1))))
   (bytes->stitch-row #"X=+++="))

  (check-equal?
   (car (values->list
         (row-squeeze (bytes->stitch-row #"X=X+++XX=")
                      '(5 4 3)
                      '(8 8 1 1)
                      '(7 6 2 0))))
   (bytes->stitch-row #"X=+++="))

  (check-equal?
   (car (values->list
         (row-squeeze (bytes->stitch-row #"=XX+++X=X")
                      '(5 4 3)
                      '(7 7 0 0)
                      '(8 6 2 1))))
   (bytes->stitch-row #"=+++=X"))

  (check-equal?
   (car (values->list
         (row-squeeze (bytes->stitch-row #"..+X-..-..+X-..-..+X-..")
                      '(2 10 18)
                      '(4 7 12 15 20)
                      '(3 11 19))))
   (bytes->stitch-row #"..+-..-..+-..-..+-.."))

  (check-equal?
   (car (values->list
         (row-squeeze (bytes->stitch-row #"..+X-..X-..+X-..X-..+X-..")
                      '(2 11 20)
                      '(4 8 13 17 22)
                      '(3 7 12 16 21))))
   (bytes->stitch-row #"..+-..X-..+-..X-..+-.."))

  (check-equal?
   (car (values->list
         (row-squeeze (bytes->stitch-row #"X-..-X+..X-..-X+..X-..-X+..")
                      '(6 15 24)
                      '(1 4 10 13 19 22)
                      '(0 5 9 14 18 23))))
   (bytes->stitch-row #"X-..-+..X-..-+..X-..-+.."))

  ;; tests of `make-stitch-matrix`
  (log-knitscheme-debug "start tests of `make-stitch-matrix` function")

  (: bytes->stitch-matrix : ((Vectorof Bytes) -> (Vectorof (Vectorof stitch))))
  (define (bytes->stitch-matrix b)
    (vector-map bytes->stitch-row b))

  #|
  (check-equal?
   (make-stitch-matrix
    (pattern #:work 'hand #:form 'circular #:face 'rs
             rows(1 4)(k6)
             rows(2 5)(k2 mr p2 ml k2)
             rows(3 6)(ssk p4 k2tog)
             rows(7)(x3(k2tog))
             rows(8)(bo)))
   '#(#"kkwkkwkk"
      #"kk:pp;kk"
      #"wUppppVw"
      #"kkwkkwkk"
      #"kk:pp;kk"
      #"wUppppVw"
      #"wwUwUwUw"
      #"wwTwTwTw"))
  |#

  (check-equal?
   (make-stitch-matrix
    (pattern #:work 'hand #:form 'circular #:face 'rs
             rows(1 7)(k7)
             rows(2)(ssk k3 k2tog)
             rows(3)(ssk k1 k2tog)
             rows(4)(k3)
             rows(5)(mr k3 ml)
             rows(6)(mr k5 ml)))
   (bytes->stitch-matrix
    '#(#"kkkkkkk"
       #"wUkkkVw"
       #"wwUkVww"
       #"wwkkkww"
       #"w:kkk;w"
       #":kkkkk;"
       #"kkkkkkk")))

  (check-equal?
   (make-stitch-matrix
    (pattern #:work 'hand #:form 'circular #:face 'rs
             rows(1 7)(k3)
             rows(2)(mr k3 ml)
             rows(3)(mr k5 ml)
             rows(4)(k7)
             rows(5)(ssk k3 k2tog)
             rows(6)(ssk k1 k2tog)))
   (bytes->stitch-matrix
    '#(#"wwkkkww"
       #"w:kkk;w"
       #":kkkkk;"
       #"kkkkkkk"
       #"wUkkkVw"
       #"wwUkVww"
       #"wwkkkww")))

  ;; tests of flat knitting options

  (check-equal?
   (make-stitch-matrix
    (pattern #:work 'hand #:form 'flat
             #:face 'rs #:side 'right
             rows(1)(p4)
             rows(2)(p1 k3)
             rows(3)(p2 k2)
             rows(4)(p3 k1)))
   (bytes->stitch-matrix
    '#(#"pppp" #"kppp" #"kkpp" #"kkkp")))

  (check-equal?
   (make-stitch-matrix
    (pattern #:work 'hand #:form 'flat
             #:face 'rs #:side 'left
             rows(1)(p4)
             rows(2)(k3 p1)
             rows(3)(k2 p2)
             rows(4)(k1 p3)))
   (bytes->stitch-matrix
    '#(#"pppp" #"kppp" #"kkpp" #"kkkp")))

  (check-equal?
   (make-stitch-matrix
    (pattern #:work 'hand #:form 'flat
             #:face 'ws #:side 'right
             rows(1)(k4)
             rows(2)(k1 p3)
             rows(3)(k2 p2)
             rows(4)(k3 p1)))
   (bytes->stitch-matrix
    '#(#"pppp" #"kppp" #"kkpp" #"kkkp")))

  (check-equal?
   (make-stitch-matrix
    (pattern #:work 'hand #:form 'flat
             #:face 'ws #:side 'left
             rows(1)(k4)
             rows(2)(p3 k1)
             rows(3)(p2 k2)
             rows(4)(p1 k3)))
   (bytes->stitch-matrix
    '#(#"pppp" #"kppp" #"kkpp" #"kkkp")))

  ;; tests of circular knitting options

  (check-equal?
   (make-stitch-matrix
    (pattern #:work 'hand #:form 'circular
             #:face 'rs #:side 'right
             rows(1)(p4)
             rows(2)(p3 k1)
             rows(3)(p2 k2)
             rows(4)(p1 k3)))
   (bytes->stitch-matrix
    '#(#"pppp" #"kppp" #"kkpp" #"kkkp")))

  (check-equal?
   (make-stitch-matrix
    (pattern #:work 'hand #:form 'circular
             #:face 'rs #:side 'left
             rows(1)(p4)
             rows(2)(k1 p3)
             rows(3)(k2 p2)
             rows(4)(k3 p1)))
   (bytes->stitch-matrix
    '#(#"pppp" #"kppp" #"kkpp" #"kkkp")))

  ;; NB this option is not available in DAK
  (check-equal?
   (make-stitch-matrix
    (pattern #:work 'hand #:form 'circular
             #:face 'ws #:side 'left
             rows(1)(k4)
             rows(2)(p1 k3)
             rows(3)(p2 k2)
             rows(4)(p3 k1)))
   (bytes->stitch-matrix
    '#(#"pppp" #"kppp" #"kkpp" #"kkkp")))

  ;; NB this option is not available in DAK
  (check-equal?
   (make-stitch-matrix
    (pattern #:work 'hand #:form 'circular
             #:face 'ws #:side 'right
             rows(1)(k4)
             rows(2)(k3 p1)
             rows(3)(k2 p2)
             rows(4)(k1 p3)))
   (bytes->stitch-matrix
    '#(#"pppp" #"kppp" #"kkpp" #"kkkp")))

  (log-knitscheme-info "knitscheme-chart tests completed"))

(log-knitscheme-info "end of knitscheme-chart.rkt")
;; end
