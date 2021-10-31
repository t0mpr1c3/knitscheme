#lang knit typed/racket
(require typed/racket)

(define _DEBUG_ #f)
(define _TEST_ #t)

(module knit-structs typed/racket
  (require typed/racket
           racket/function ; needed for `curry`
           racket/list) ; needed for `flatten`, `range`
  (provide (all-defined-out))
  
  (define _DEBUG_MODULE_ #t)
 
  ;; restrict list to unique elements
  (: uniq : (All (A) (Listof A) -> (Listof A)))
  (define (uniq [xs : (Listof A)])
    (cond [(null? xs) null]
          [(member (car xs) (cdr xs)) (uniq (cdr xs))]
          [else (cons (car xs) (uniq (cdr xs)))]))

  ;; apply function to consecutive elements of list
  (: diff : (All (A) ((-> A A A) (Listof A) -> (Listof A))))
  (define (diff f xs)
    (for/list ([x xs] [y (cdr xs)])
      (f y x)))

  ;; sum of elements
  (: sum : ((Listof Number) -> Number))
  (define (sum xs)
    (foldl + 0 xs))

  ;; index of first non-false element of vector
  (: vector-which : ((Vectorof Any) -> (U Index False)))
  (define (vector-which xs)
    (let ([res
           (for/list : (Listof Index)
             ([i (vector->list xs)]
              [j (range (vector-length xs))]
              #:when (not (equal? i #f)))
             (cast j Index))])
      (if (null? res)
          #f
          (car res))))
  
  ;; stitch definitions
  ;; NB symbols are based on Stitchmastery Dash font
 
  ;; type of stitch
  (struct stitchtype
    ([rs-symbol : Byte]
     [ws-symbol : Byte]
     [stitches-in : Index]
     [stitches-out : Index]
     [id : String]
     [name : String]
     [rx : PRegexp]
     [repeatable : Boolean]
     [variable-repeat : Boolean])
    #:prefab)

  (define stitch-list : (Listof stitchtype)
    (list

     ;; variable repeat stitches
     (stitchtype #x6b #x70 1 1 "k"    "knit"       #px"k(nit ?)?_" #t #t)
     (stitchtype #x70 #x6b 1 1 "p"    "purl"       #px"p(url ?)?_" #t #t)
     (stitchtype #x6e #x3f 1 1 "ktbl" "knit-tbl"   #px"k(nit ?)?_ ?tbl" #t #t)
     (stitchtype #x3f #x6e 1 1 "ptbl" "purl-tbl"   #px"p(url ?)?_ ?tbl" #t #t)
     (stitchtype #x21 #x25 1 1 "kb"   "knit-below" #px"k(nit ?)?_ ?b(elow)?" #t #t)
     (stitchtype #x25 #x21 1 1 "pb"   "purl-below" #px"p(url ?)?_ ?b(elow)?" #t #t)
     (stitchtype #x54 #x54 1 0 "bo"   "bind-off"   #px"b(ind ?off|o) ?_" #t #t)

     ;; repeatable stitches; 
     (stitchtype #x2a #x51 1 1 "slwyib" "slip-wyib" #px"sl(ip ?)?_ ?wyib" #t #f)
     (stitchtype #x51 #x2a 1 1 "slwyif" "slip-wyif" #px"sl(ip ?)?_ ?wyif" #t #f)
     (stitchtype #xbc #xbc 0 1 "co"     "cast-on"   #px"c(ast ?on|o) ?_" #t #f)

     ;; unrepeatable stitches
     (stitchtype #x2c #x2c 1 0 "drop"  "drop-stitch"              #px"drop st(itch)" #f #f)
     (stitchtype #x55 #x57 2 1 "k2tog" "knit-two-together"        #px"k(nit ?)?2 ?tog(ether)?" #f #f)
     (stitchtype #x57 #x55 2 1 "p2tog" "purl-two-together"        #px"p(url ?)?2 ?tog(ether)?" #f #f)
     (stitchtype #x73 #x75 3 1 "k3tog" "knit-three-together"      #px"k(nit ?)?3 ?tog(ether)?" #f #f)
     (stitchtype #x75 #x73 3 1 "p3tog" "purl-three-together"      #px"p(url ?)?3 ?tog(ether)?" #f #f)
     (stitchtype #x56 #x58 2 1 "ssk"   "slip-slip-knit"           #px"ssk" #f #f)
     (stitchtype #x58 #x56 2 1 "ssp"   "slip-slip-purl"           #px"ssp" #f #f)
     (stitchtype #x74 #x76 3 1 "sssk"  "slip-slip-slip-knit"      #px"sssk" #f #f)
     (stitchtype #x76 #x74 3 1 "sssp"  "slip-slip-slip-purl"      #px"sssp" #f #f)
     (stitchtype #x6f #x6f 0 1 "yo"    "yarn-over"                #px"y(arn ?over|o)" #f #f)
     (stitchtype #x41 #x41 0 2 "dyo"   "double-yarn-over"         #px"d((ouble|bl) yarn ?over|yo)" #f #f)
     (stitchtype #x6a #x6a 3 1 "cdd"   "centered-double-decrease" #px"c((enter(ed)?|tr) d(ouble|bl) dec(rease)?|dd)" #f #f)
     (stitchtype #x3a #x78 0 1 "ml"    "make-left"                #px"m(ake ?)?_ ?l(eft)?" #f #f)
     (stitchtype #x78 #x3a 0 1 "mlp"   "make-left-purlwise"       #px"m(ake ?)?_ ?l((eft)? (pw?|url(wise)?)|p)" #f #f)
     (stitchtype #x3b #x79 0 1 "mr"    "make-right"               #px"m(ake ?)?_ ?r(ight)?" #f #f)
     (stitchtype #x79 #x3b 0 1 "mrp"   "make-right-purlwise"      #px"m(ake ?)?_ ?r((ight)? (pw?|url(wise)?)|p)" #f #f)
     (stitchtype #x3e #x40 0 1 "m"     "make"                     #px"m(ake ?)?_" #f #f)
     (stitchtype #x40 #x3e 0 1 "mp"    "make-purlwise"            #px"m(ake ?)?_ ?(pw?|url(wise)?)" #f #f)
     (stitchtype #x4c #x7d 1 3 "kyk"   "k1-yo-k1-in-next-stitch"  #px"k(nit ?)?1,? y(arn ?over|o),? k(nit ?)?1 in(to)? next st(itch)?" #f #f)
     (stitchtype #x7d #x4c 1 3 "pyp"   "p1-yo-p1-in-next-stitch"  #px"p(url ?)?1,? y(arn ?over|o),? p(url ?)?1 in(to)? next st(itch)?" #f #f)
     (stitchtype #x69 #x69 1 3 "cdi"   "centered-double-increase" #px"c((enter(ed)?|tr) d(ouble|bl) inc(rease)?|dd)" #f #f)
     (stitchtype #xbf #xbf 1 1 "mb"    "make-bobble"              #px"m(ake bobble|b)" #f #f)
     (stitchtype #x50 #x4f 0 0 "w&t"   "wrap-and-turn"            #px"w(rap( and |&)turn|&t)" #f #f)))

  (define stitch-hash
    (for/hash : (HashTable Byte stitchtype) ([i (in-range (length stitch-list))])
      (values (stitchtype-rs-symbol (list-ref stitch-list i))
              (list-ref stitch-list i))))
  
  ;; stitch struct
  (struct stitch
    ([stitchtype : Byte]
     [yarntype : Byte])
    #:prefab)

  (: stitch->char : (stitch -> Char))
  (define (stitch->char st)
    (integer->char (stitch-stitchtype st)))
  
  ;; stitch information in each row is encoded in a tree structure:
  ;; leaf data = sequence of stitches
  ;; node data = number of repeats
  ;; each node can have multiple children

  ;; recursive definition of tree
  (define-type Tree (Listof (U Node Leaf)))
  (define-type Leaf (Pairof Index stitch)) ; repeat-count stitch
  (define-type Node (Pairof Index Tree)) ; repeat-count repeat-content

  ;; definition of repeat as node of stitches
  (: repeat : (Index (U Node Leaf) (U Node Leaf) * -> Node))
  (define (repeat n . xs)
    (make-node n xs))

  ;; aliases for small number repeats

  ;;(require srfi/26) ; needed for `cut`
  ;;(define times (lambda (n) (cut repeat n <...>)))

  (define times (curry repeat)) 
  (define once (times 1))
  (define twice (times 2))
  (define one (times 1))
  (define two (times 2))
  (define three (times 3))
  (define four (times 4))
  (define five (times 5))
  (define six (times 6))
  (define seven (times 7))
  (define eight (times 8))
  (define nine (times 9))
  (define ten (times 10))
  (define eleven (times 11))
  (define twelve (times 12))
  (define thirteen (times 13))
  (define fourteen (times 14))
  (define fifteen (times 15))
  (define sixteen (times 16))
  (define seventeen (times 17))
  (define eighteen (times 18))
  (define nineteen (times 19))
  (define twenty (times 20))

  ;; function for defining sequences of row numbers
  (: sequence : (->* (Positive-Index) (Positive-Index Positive-Index) (Listof Positive-Index)))
  (define sequence
    (case-lambda
      [([x : Positive-Index]) (cast (range 1 (add1 x)) (Listof Positive-Index))]
      [([x : Positive-Index]
        [y : Positive-Index]) (cast (range x (add1 y)) (Listof Positive-Index))]
      [([x : Positive-Index]
        [y : Positive-Index]
        [z : Positive-Index]) (cast (range x (add1 y) z) (Listof Positive-Index))]
      [else (raise "error")]))
  (define seq sequence)
  

  ;; macro definitions
  
  (begin-for-syntax
    (require racket/list ; needed for `range`
             racket/syntax)) ; needed for `format-id`

  ;; define x1 ... x20
  (define-syntax (define-xns stx)
    (syntax-case stx ()
      [(_) (let ([xn-id (lambda (i) (format-id stx "x~a" i))])
             (with-syntax ([((n xn) ...) (map (lambda (j) (list j (xn-id j))) (range 1 21))])
               #'(begin
                   (define xn (times n))
                   ...)))]))
  (define-xns)

  ;; define symbol functions
  (define-syntax-rule (define-symbolfunc id)
    (define-syntax (id stx)
      (syntax-case stx ()
        [_ #'(string->symbol (format "~a" 'id))])))
  (define-symbolfunc hand)
  (define-symbolfunc machine)
  (define-symbolfunc left)
  (define-symbolfunc right)
  (define-symbolfunc rs)
  (define-symbolfunc ws)

  ;; yarn symbol generator
  (: yarn : (Byte -> Symbol))
  (define (yarn n)
    (if (zero? n)
        'mc
        (string->symbol (format "cc~a" n))))

  ;; define yarn symbols mc ... cc20
  (define-symbolfunc mc)
  (define-syntax (define-ccns stx)
    (syntax-case stx ()
      [(_) (let ([ccn-id (lambda (i) (format-id stx "cc~a" i))])
             (with-syntax ([((n ccn) ...) (map (lambda (j) (list j (ccn-id j))) (range 1 21))])
               #'(begin
                   (define ccn (yarn n))
                   ...)))]))
  (define-ccns)

  ;; end of macro definitions
  
  
  ;; leaf functions
  (define-predicate Leaf? Leaf)

  (: make-leaf : (Index stitch -> Leaf))
  (define (make-leaf n st)
    (cons n st))
  
  (: leaf-count : (Leaf -> Index))
  (define (leaf-count leaf)
    (car leaf))
  
  (: leaf-stitch : (Leaf -> stitch))
  (define (leaf-stitch leaf)
    (cdr leaf))

  (: leaf-stitchtype : (Leaf -> Byte))
  (define (leaf-stitchtype leaf)
    (stitch-stitchtype (leaf-stitch leaf)))

  (: leaf-yarntype : (Leaf -> Byte))
  (define (leaf-yarntype leaf)
    (stitch-yarntype (leaf-stitch leaf)))
  
  ;; count stitches consumed by leaf (excluding variable repeats)
  (: leaf-stitches-in : (Leaf -> Nonnegative-Integer))
  (define (leaf-stitches-in leaf)
    (* (leaf-count leaf) (stitchtype-stitches-in (hash-ref stitch-hash (leaf-stitchtype leaf)))))

  ;; count stitches produced by leaf (excluding variable repeats)
  (: leaf-stitches-out : (Leaf -> Nonnegative-Integer))
  (define (leaf-stitches-out leaf)
    (* (leaf-count leaf) (stitchtype-stitches-out (hash-ref stitch-hash (leaf-stitchtype leaf)))))

  ;; node functions
  (: make-node : (Index Tree -> Node))
  (define (make-node n tree)
    (cons n tree))
  
  (: node-count : (Node -> Index))
  (define (node-count node)
    (car node))
  
  (: node-tree : (Node -> Tree))
  (define (node-tree node)
    (cdr node))

  ;; tree functions
  (: make-tree : ((U Node Leaf) (U Node Leaf) * -> Tree))
  (define (make-tree . xs) xs)

  ;; count variable repeats in tree
  (: tree-count-var : (Tree -> Nonnegative-Integer))
  (define (tree-count-var tree)
    (foldl (lambda ([x : (U Node Leaf)]
                    [y : Nonnegative-Integer])
             (if (Leaf? x)
                 (if (zero? (leaf-count x))
                     (add1 y)
                     y)
                 (if (zero? (node-count x))
                     (+ 1 y (tree-count-var (node-tree x)))
                     (+   y (tree-count-var (node-tree x))))))
           0
           tree))

  ;; obtain (leftmost non-nested) variable repeat from tree
  ;; or return #f if no variable repeat
  (: tree-var : (Tree -> (U Tree False)))
  (define (tree-var tree)
    (for/or : (U Tree False) ([i (in-range (length tree))])
      (let ([x : (U Node Leaf) (list-ref tree i)])
        (if (Leaf? x)
            (if (zero? (leaf-count x))
                (make-tree (make-leaf 1 (leaf-stitch x)))
                #f)
            (if (zero? (node-count x))
                (make-tree (make-node 1 (node-tree x)))
                (tree-var (node-tree x)))))))

  ;; replace variable repeat(s) in tree with fixed integer value
  ;; if there are nested repeats, only the lowest will be replaced
  (: tree-var-replace : (Tree Index -> Tree))
  (define (tree-var-replace tree r) : Tree
    (reverse
     (foldl (lambda ([x : (U Node Leaf)]
                     [y : Tree])
              (if (Leaf? x)
                  (if (zero? (leaf-count x))
                      (cons (make-leaf r (leaf-stitch x)) y)
                      (cons x y))
                  (if (zero? (node-count x))
                      (cons (make-node r (node-tree x)) y)
                      (cons (make-node (node-count x) (tree-var-replace (node-tree x) r)) y))))
            (cast null Tree)
            tree)))
  
  ;; flatten tree to a list of leaves
  (: flatten-tree : (Tree -> (Listof Leaf)))
  (define (flatten-tree tree)
    (combine (cast (flatten-tree-recurse tree) (Listof Leaf))))
  
  ;; combine consecutive leaves with same stitch type into single leaf
  (: combine : ((Listof Leaf) -> (Listof Leaf)))
  (define (combine lst)
    (reverse
     (foldl (lambda ([x : Leaf]
                     [y : (Listof Leaf)])
              (if (zero? (leaf-count x)) 
                  ;; zero number elements are ignored
                  y
                  ;; FIXME an error could be raised instead
                  ;; because elsewhere 0 signifies a variable number element
                  (if (null? y)
                      (list x)
                      (if (equal? (leaf-stitch x) (leaf-stitch (car y)))
                          (cons (make-leaf (cast (+ (leaf-count x) (leaf-count (car y))) Index)
                                           (leaf-stitch x))
                                (cdr y))
                          (cons x y)))))
            null
            lst))) 
    
  ;; flatten every node in tree to list of leaves
  (: flatten-tree-recurse : (Tree -> Tree))
  (define (flatten-tree-recurse tree)
    (reverse
     (foldl (lambda ([x : (U Node Leaf)]
                     [y : Tree])
              (if (Leaf? x)
                  (cons x y)
                  (append (apply append (build-list (node-count x)
                                                    (lambda (z)
                                                      (flatten-tree-recurse (node-tree x)))))
                          y)))
            (cast null Tree)
            tree)))
  
  ;; tests
  (when _DEBUG_MODULE_
    (define t1
      (make-tree (make-leaf 2 (stitch #x70 0))
                 (make-node 0 (make-tree (make-leaf 1 (stitch #x6f 0))
                                         (make-node 3 (make-tree (make-leaf 2 (stitch #x54 0))))
                                         (make-leaf 2 (stitch #x6f 0))))
                 (make-leaf 3 (stitch #x6a 0))))
    (displayln t1)
    (displayln (flatten-tree t1))
    (displayln (tree-var-replace t1 2))
    (tree-count-var t1)
    (tree-var t1)
    (define t2
      (make-tree (make-leaf 0 (stitch #x70 0))
                 (make-leaf 0 (stitch #x6f 0))))
    (displayln t2)
    (displayln (flatten-tree t2))
    (displayln (tree-var-replace t2 2))
    (tree-count-var t2)
    (tree-var t2))
  
  
  ;; define data structs

  ;; non-empty list
 
  ;; yarn
  (struct yarntype
    ([brand : String]
     [fiber : String]
     [weight : String]
     [color : String])
    #:prefab)
  
  (: consecutive-rows : (-> (Listof Positive-Index) Boolean))
  (define (consecutive-rows rownums)
    (and
     (> (length rownums) 1)
     (= 1 (apply min (diff - rownums)))))
                            
  ;; row struct
  (struct Rows
    ([rownums : (Listof Positive-Index)]
     [stitches : Tree]
     [memo : String]
     [stitches-in-fixed : Natural]
     [stitches-out-fixed : Natural]
     [stitches-in-var : Natural]
     [stitches-out-var : Natural]
     [stitches-in-total : Natural]
     [stitches-out-total : Natural])
    #:guard
    (lambda (rownums
             stitches
             memo
             stitches-in-fixed
             stitches-out-fixed
             stitches-in-var
             stitches-out-var
             stitches-in-total
             stitches-out-total
             type-name)
      ;; flatten and sort list of row numbers
      (let ([rownums~ (sort ((inst uniq (Listof Positive-Index)) (flatten rownums)) <)]
            [var-count (tree-count-var stitches)]
            [flat (flatten-tree stitches)])
        (when _DEBUG_MODULE_
          (displayln "In struct Rows")
          (displayln (~a rownums))
          (displayln (~a stitches))
          (displayln (~a stitches-in-fixed))
          (displayln (~a stitches-out-fixed))
          (displayln (~a stitches-in-var))
          (displayln (~a stitches-out-var))
          (displayln (~a stitches-in-total))
          (displayln (~a stitches-out-total)))
        ;; check valid row numbers exist
        (if (zero? (length rownums~))
            (raise "Error: no row numbers specified")
            ;; check no more than one variable repeat
            (if (> var-count 1)
                (raise "Error: more than one variable number repeat specified")
                (let ([stitches-in-fixed~ (sum (map leaf-stitches-in flat))]
                      [stitches-out-fixed~ (sum (map leaf-stitches-out flat))])
                  (if (zero? var-count)
                      ;; no variable repeats
                      (if (and
                           (not (zero? stitches-out-total))
                           (not (= stitches-out-fixed~ stitches-out-total)))
                          (raise "Error: total number of stitches produced does not match stitches in row")
                          ;; result
                          (values rownums~
                                  stitches
                                  memo
                                  stitches-in-fixed~
                                  stitches-out-fixed~
                                  0
                                  0
                                  stitches-in-fixed~
                                  stitches-out-fixed~))
                      ;; one variable repeat
                      (let* ([flat-var (flatten-tree (tree-var stitches))]
                             [stitches-in-var~ (sum (map leaf-stitches-in flat-var))]
                             [stitches-out-var~ (sum (map leaf-stitches-out flat-var))])
                        ;; #:stitches has been specified
                        ;; check that variable number repeat can be replaced by fixed value
                        (if (zero? stitches-out-var~)                     
                            (raise "Error: total number of stitches produced does not conform with variable number repeat")
                            (let-values ([(q r)
                                          (quotient/remainder (- stitches-out-total stitches-out-fixed~)
                                                              stitches-out-var~)])
                              (if (not (zero? r))
                                  (raise "Error: total number of stitches produced does not conform with variable number repeat")
                                  (if (consecutive-rows rownums~)
                                      ;; consecutive rows
                                      ;; replace variable number repeat by fixed value
                                      (let* ([stitches~ (tree-var-replace stitches q)]
                                             [flat~ (flatten-tree stitches~)]
                                             [stitches-in-total~ (sum (map leaf-stitches-in flat~))])
                                        ;; check that stitches in/out are conformable for consecutive rows
                                        (if (not (= stitches-in-total~ stitches-out-total))                 
                                            (raise "Error: consecutive rows are not conformable")
                                            ;; result
                                            (values rownums~
                                                    stitches~
                                                    memo
                                                    stitches-out-total
                                                    stitches-out-total
                                                    0
                                                    0
                                                    stitches-out-total
                                                    stitches-out-total)))
                                      ;; no consecutive rows
                                      (values rownums~
                                              stitches
                                              memo
                                              stitches-in-fixed~
                                              stitches-out-fixed~
                                              stitches-in-var~
                                              stitches-out-var~
                                              stitches-in-total
                                              stitches-out-total)))))
                        ;; #:stitches has not been specified
                        (let ([diff1 (abs (- stitches-in-fixed~ stitches-out-fixed~))]
                              [diff2 (abs (- stitches-in-var~ stitches-out-var~))])
                          ;; check for consecutive rows
                          (if (and
                               (consecutive-rows rownums~)
                               ;; check that stitches in/out are conformable for consecutive rows
                               ;; conformable means that the difference between the stitches in/out
                               ;; is an integer multiple of the difference between the variable stitches in/out
                               (not (zero? diff1))
                               (or (zero? diff2)
                                   (not (zero? (remainder diff1 diff2)))))                      
                              (raise "Error: consecutive rows are not conformable")
                              ;; result
                              (values rownums~
                                      stitches
                                      memo
                                      stitches-in-fixed~
                                      stitches-out-fixed~
                                      stitches-in-var~
                                      stitches-out-var~
                                      0
                                      0))))))))))
    #:transparent)

  #|
  ;; FIXME need to define macro to insert memo keyword
  (: memo (-> String Symbol))
  (define (memo m) (string->symbol m))
  |#

  ;; alternative constructor
  (: rows : (->* () (#:memo String #:stitches Nonnegative-Integer) #:rest (U Positive-Index (Listof Positive-Index)) 
                 (-> (U Leaf Node) (U Leaf Node) * Rows)))
  (define ((rows #:memo [memo : String ""] #:stitches [stitches-out-total : Nonnegative-Integer 0] . rownums) . tree)
    (Rows
     (cast (flatten rownums) (Listof Positive-Index))
     tree
     memo
     0 0 0 0 0
     stitches-out-total))
  ;; aliases
  (define row rows)
  (define round rows)
  (define rounds rows)

  #|
  ;; tests
  ;; rows not consecutive
  rows(1 3)((list-ref t1 0)
            (list-ref t1 2))
  ;; consecutive and conformable
  rows(1 2)((list-ref t1 0)
            (list-ref t1 0))
  ;; consecutive and conformable
  rows('(1 2) '(1) #:memo "new memo")((list-ref t1 0)
                                      (list-ref t1 1)
                                      (list-ref t1 2))
  ;; consecutive but not conformable
  rows(1 2)((list-ref t1 0)
            (list-ref t1 2))
  ;; consecutive but not conformable
  rows(1 2)((list-ref t1 0)
            (list-ref t1 1)
            (make-leaf 1 (stitch #x6a 0)))
  ;; no row numbers
  rows(null)((list-ref t1 0))
  |#
  
  ;; information for one row/round
  ;; NB unlike Knitspeak, we do not encode Row/Round info for each row.
  ;; The whole pattern is either flat or circular.
  ;; Likewise, RS/WS information is not encoded at the row level.
  ;; RS/WS is specified only for Row 1, and alternates for flat patterns.
  ;; These are generally sensible restrictions, justifiable w.r.t. existing patterns.
  (struct Rowinfo
    ([stitches : Tree]
     [memo : String]
     [stitches-in-fixed : Natural]   ; only used in
     [stitches-out-fixed : Natural]  ; guard function
     [stitches-in-var : Natural]     ; and subsequently
     [stitches-out-var : Natural]    ; set to zero
     [stitches-in-total : Natural]
     [stitches-out-total : Natural])
    #:transparent)
  
  ;; 2D vector:
  ;; D1: rowinfo index
  ;; D2: vector of row numbers (1-indexed)
  (struct
    Rowmap
    ([data : (Immutable-Vectorof (Immutable-Vectorof Positive-Index))]
     [rowcount : Positive-Integer])
    #:guard
    ;; check validity of row numbers
    (lambda (data rowcount type-name)
      (let ([fl : (Listof Positive-Index) (vector->list (apply vector-append (vector->list data)))])
        (if (> (apply min fl) 1)
            (raise "Error: row numbers must start at 1")
            (if (or
                 (equal? fl '(1))
                 (equal? (uniq (diff - (sort fl <))) '(1)))
                (values data (apply max fl))
                (raise "Error: pattern must specify consecutive row numbers")))))
    #:transparent)

  ;; alternative constructor
  (define (rowmap
           [data : (Immutable-Vectorof (Immutable-Vectorof Positive-Index))]
           [rowcount : Positive-Integer 1]) : Rowmap
    (Rowmap data 1))

  ;; find row number as rowmap index
  (define (rowmap-find
           [rowmap : Rowmap]
           [r : Positive-Index]) : (U Index False)
    (vector-which
     (vector-map
      (lambda ([rs : (Vectorof Positive-Index)]) (vector-memq r rs))
      (Rowmap-data rowmap))))
  
  ;; gauge
  (struct Gauge
    ([st-count-x : Positive-Float]
     [st-count-y : Positive-Float]
     [measurement-x : Positive-Float]
     [measurement-y : Positive-Float]
     [unit : (U 'cm 'inch)])
    #:prefab)
  
  ;; pattern struct
  (struct Pattern
    ([rowinfo : (Vectorof Rowinfo)]
     [rownums : Rowmap]
     [technology : (U 'hand 'machine)]
     [geometry : (U 'flat 'circular)]
     [startface : (U 'rs 'ws)]
     [startside : (U 'left 'right)]
     [gauge : (Option Gauge)]
     [yarntype : (Immutable-Vectorof (Option yarntype))])
    #:guard
    (lambda (rowinfo rownums technology geometry startface startside gauge yarntype type-name)
      ;; circular for hand knits only
      (if (and
           (eq? technology 'machine)
           (eq? geometry 'circular))
          (raise "Error: machine knit patterns must be flat, not circular")
          ;; make vectors of stitch totals
          (let ([n-rows : Index (Rowmap-rowcount rownums)])
            (let ([stitches-in-fixed ((inst make-vector Natural) n-rows 0)]
                  [stitches-out-fixed ((inst make-vector Natural) n-rows 0)]
                  [stitches-in-var ((inst make-vector Natural) n-rows 0)]
                  [stitches-out-var ((inst make-vector Natural) n-rows 0)]
                  [stitches-in-total ((inst make-vector Natural) n-rows 0)]
                  [stitches-out-total ((inst make-vector Natural) n-rows 0)]
                  [var-count ((inst make-vector Natural) n-rows 0)])
              (for ([i (in-range (vector-length (Rowmap-data rownums)))])
                (let ([row-i (vector-ref rowinfo i)]
                      [rownums-i : (Immutable-Vectorof Index) (vector-ref (Rowmap-data rownums) i)])
                  (let ([stitches-in-fixed-row-i (Rowinfo-stitches-in-fixed row-i)]
                        [stitches-out-fixed-row-i (Rowinfo-stitches-out-fixed row-i)]
                        [stitches-in-var-row-i (Rowinfo-stitches-in-var row-i)]
                        [stitches-out-var-row-i (Rowinfo-stitches-out-var row-i)]
                        [stitches-in-total-row-i (Rowinfo-stitches-in-total row-i)]
                        [stitches-out-total-row-i (Rowinfo-stitches-out-total row-i)]                
                        [var-count-row-i ((compose1 tree-count-var Rowinfo-stitches) row-i)])
                    (for ([j (in-range (vector-length rownums-i))])
                      (vector-set! stitches-in-fixed (sub1 (vector-ref rownums-i j)) stitches-in-fixed-row-i)
                      (vector-set! stitches-out-fixed (sub1 (vector-ref rownums-i j)) stitches-out-fixed-row-i)
                      (vector-set! stitches-in-var (sub1 (vector-ref rownums-i j)) stitches-in-var-row-i)
                      (vector-set! stitches-out-var (sub1 (vector-ref rownums-i j)) stitches-out-var-row-i)
                      (vector-set! stitches-in-total (sub1 (vector-ref rownums-i j)) stitches-in-total-row-i)
                      (vector-set! stitches-out-total (sub1 (vector-ref rownums-i j)) stitches-out-total-row-i)
                      (vector-set! var-count (sub1 (vector-ref rownums-i j)) var-count-row-i)))))
              (when _DEBUG_MODULE_
                (displayln "In struct Pattern")
                (displayln (~a rowinfo)))
              ;; constrain adjacent rows
              (for ([i (in-range 1 n-rows)])
                (let ([j (sub1 i)])
                  (when (zero? (vector-ref stitches-in-total i))
                    (if (and
                         (zero? (vector-ref var-count i))
                         (not (= (vector-ref stitches-out-total j)
                                 (vector-ref stitches-in-fixed i))))
                        (raise (format "Error: pattern rows ~a and ~a are not conformable" i (add1 i)))
                        (vector-set! stitches-in-total i
                                     (vector-ref stitches-out-total j))))
                  (if (zero? (vector-ref stitches-out-total j))
                      (if (and
                           (zero? (vector-ref var-count j))
                           (not (= (vector-ref stitches-in-total i)
                                   (vector-ref stitches-out-fixed j))))
                          (raise (format "Error: pattern rows ~a and ~a are not conformable" i (add1 i)))
                          (vector-set! stitches-out-total j
                                       (vector-ref stitches-in-total i)))
                      (when (not (= (vector-ref stitches-in-total i)
                                    (vector-ref stitches-out-total j)))
                        (raise (format "Error: pattern rows ~a and ~a are not conformable" i (add1 i)))))))
              ;; constrain variable repeats
              (for ([i (in-range 1 (add1 n-rows))])
                (let* ([j (sub1 i)]
                       [var-count-j (vector-ref var-count j)])
                  (when (not (zero? var-count-j))
                    (if (> var-count-j 1)
                        (raise (format "Error: more than one variable number repeat specified in row ~a" i))
                        ;; one variable repeat in row (j+1)
                        (let ([in-total (vector-ref stitches-in-total j)]
                              [out-total (vector-ref stitches-out-total j)])
                          (if (and
                               (zero? in-total)
                               (zero? out-total))
                              ;; no constraints
                              (raise (format "Error: unconstrained variable repeat in row ~a" i))                    
                              ;; constrain variable repeat
                              (let ([in-fixed (vector-ref stitches-in-fixed j)]
                                    [out-fixed (vector-ref stitches-out-fixed j)]
                                    [in-var (vector-ref stitches-in-var j)]
                                    [out-var (vector-ref stitches-out-var j)])
                                (let ([qi (quotient (- in-total in-fixed) in-var)]
                                      [qo (quotient (- out-total out-fixed) out-var)])
                                  (when (and
                                         (not (zero? in-total))
                                         (not (zero? out-total))
                                         (not (= qi qo)))
                                    (raise (format "Error: incompatible constraints in row ~a" i)))
                                  (when (not (zero? in-total))
                                    (let ([k (rowmap-find rownums i)])
                                      (if (not k)
                                          (raise (format "Error: could not find row ~a in pattern" i))
                                          (let* ([rowinfo~ (vector-ref rowinfo k)]
                                                 [stitches~ (tree-var-replace (Rowinfo-stitches rowinfo~) qi)]
                                                 [out-total~ (+ out-fixed (* qi out-var))])                                              
                                            (vector-set! rowinfo k
                                                         (Rowinfo
                                                          stitches~
                                                          (Rowinfo-memo rowinfo~)
                                                          0
                                                          0
                                                          0
                                                          0
                                                          in-total
                                                          out-total~))))))
                                  (when (not (zero? in-total))
                                    (let ([k (rowmap-find rownums i)])
                                      (if (not k)
                                          (raise (format "Error: could not find row ~a in pattern" i))
                                          (let* ([rowinfo~ (vector-ref rowinfo k)]
                                                 [stitches~ (tree-var-replace (Rowinfo-stitches rowinfo~) qi)]
                                                 [in-total~ (+ in-fixed (* qo in-var))])                                              
                                            (vector-set! rowinfo k
                                                         (Rowinfo
                                                          stitches~
                                                          (Rowinfo-memo rowinfo~)
                                                          0
                                                          0
                                                          0
                                                          0
                                                          in-total~
                                                          out-total))))))))))))))                  
              ;; check that consecutive rows are conformable
              (if (and (> n-rows 1)
                       (for/or ([i (in-range 1 n-rows)])
                         (not (= (vector-ref stitches-in-total i) (vector-ref stitches-out-total (sub1 i))))))
                  (raise "Error: pattern rows not conformable")
                  ;; result
                  (values rowinfo rownums technology geometry startface startside gauge yarntype))))))
    #:transparent)

  #|
  (: pattern-rows-apply : (All (A) ((Rowinfo -> A) Pattern -> (Mutable-Vectorof (U Zero A)))))
  (define (pattern-rows-apply f pattern)
    (let ([res ((inst make-vector (U Zero A)) (Rowmap-rowcount (Pattern-rownums pattern)) 0)])
      (for ([i (in-range (vector-length (Rowmap-data (Pattern-rownums pattern))))])
        (let ([row-i (vector-ref (Pattern-rowinfo pattern) i)]
              [rownums-i : (Immutable-Vectorof Index) (vector-ref (Rowmap-data (Pattern-rownums pattern)) i)])
          (let ([f-row-i (f row-i)])
            (for ([j (in-range (vector-length rownums-i))])
              (vector-set! res (sub1 (vector-ref rownums-i j)) f-row-i)))))
      res))
  |#
  
  ;; alternative constructor
  (: pattern : (->* () (#:technology (U 'hand 'machine)
                        #:geometry (U 'flat 'circular)
                        #:startface (U 'rs 'ws)
                        #:startside (U 'left 'right)
                        #:gauge (Option Gauge)
                        #:yarntype (Immutable-Vectorof (Option yarntype)))
                    #:rest Rows
                    Pattern))
  (define (pattern
           #:technology [technology : (U 'hand 'machine) 'machine]
           #:geometry [geometry : (U 'flat 'circular) 'flat]
           #:startface [startface : (U 'rs 'ws) 'rs]
           #:startside [startside : (U 'left 'right) 'right]
           #:gauge [gauge : (Option Gauge) #f]
           #:yarntype [yarntype : (Immutable-Vectorof (Option yarntype)) (vector-immutable #f)]
           . rows)
    (let ([rowinfo~
           (list->vector
            (reverse
             (foldl (lambda ([x : Rows]
                             [y : (Listof Rowinfo)])
                      (cons (Rowinfo
                             (Rows-stitches x)
                             (Rows-memo x)
                             (Rows-stitches-in-fixed x)
                             (Rows-stitches-out-fixed x)
                             (Rows-stitches-in-var x)
                             (Rows-stitches-out-var x)
                             (Rows-stitches-in-total x)
                             (Rows-stitches-out-total x))
                            y))
                    null
                    rows)))]
          [rowmap~
           (rowmap
            (vector->immutable-vector
             ((inst list->vector (Immutable-Vectorof Positive-Index))
              (reverse
               (foldl (lambda ([x : Rows]
                               [y : (Listof (Immutable-Vectorof Positive-Index))])   
                        (cons
                         (vector->immutable-vector
                          ((inst list->vector Positive-Index)
                           (Rows-rownums x)))
                         y))
                      null
                      rows))))
            1)])
      (when _DEBUG_MODULE_
        (displayln "In function pattern")
        (displayln (~a rows))
        (displayln (~a rowinfo~))
        (displayln (~a rowmap~)))
      ;; result
      (Pattern rowinfo~ rowmap~ technology geometry startface startside gauge yarntype)))

  ;; end of module
  )

(require (submod "." knit-structs))

; macro definitions

(define-syntax-rule (define-repeatable-stitch id st)
  (define-syntax-rule (id n)
    ;(make-string n (integer->char (stitchtype-rs-symbol st)))))
    (make-leaf n (stitch (stitchtype-rs-symbol st) 0))))

(define-syntax-rule (define-unrepeatable-stitch id st)
  (define-syntax (id stx)
    (syntax-case stx ()
      ;[_ #'(string (integer->char (stitchtype-rs-symbol st)))])))
      [_ #'(make-leaf 1 (stitch (stitchtype-rs-symbol st) 0))])))

#|
(module+ test
  (require rackunit)
  (check-equal? x3("-") "---"))
|#

; stitch function definitions
  

(define-repeatable-stitch k       (hash-ref stitch-hash #x6b))
(define-repeatable-stitch p       (hash-ref stitch-hash #x70))
(define-repeatable-stitch ktbl    (hash-ref stitch-hash #x6e))
(define-repeatable-stitch ptbl    (hash-ref stitch-hash #x3f))
(define-repeatable-stitch kb      (hash-ref stitch-hash #x21))
(define-repeatable-stitch pb      (hash-ref stitch-hash #x25))
(define-repeatable-stitch bo      (hash-ref stitch-hash #x54))

(define-repeatable-stitch slwyib  (hash-ref stitch-hash #x2a))
(define-repeatable-stitch slwyif  (hash-ref stitch-hash #x51))
(define-repeatable-stitch co      (hash-ref stitch-hash #xbc))

(define-unrepeatable-stitch drop  (hash-ref stitch-hash #x2c))
(define-unrepeatable-stitch k2tog (hash-ref stitch-hash #x55))
(define-unrepeatable-stitch p2tog (hash-ref stitch-hash #x57))
(define-unrepeatable-stitch k3tog (hash-ref stitch-hash #x73))
(define-unrepeatable-stitch p3tog (hash-ref stitch-hash #x75))
(define-unrepeatable-stitch ssk   (hash-ref stitch-hash #x56))
(define-unrepeatable-stitch ssp   (hash-ref stitch-hash #x58))
(define-unrepeatable-stitch sssk  (hash-ref stitch-hash #x74))
(define-unrepeatable-stitch sssp  (hash-ref stitch-hash #x76))
(define-unrepeatable-stitch cdd   (hash-ref stitch-hash #x6f))
(define-unrepeatable-stitch yo    (hash-ref stitch-hash #x41))
(define-unrepeatable-stitch dyo   (hash-ref stitch-hash #x6a))
(define-unrepeatable-stitch ml    (hash-ref stitch-hash #x3a))
(define-unrepeatable-stitch mlp   (hash-ref stitch-hash #x78))
(define-unrepeatable-stitch mr    (hash-ref stitch-hash #x3b))
(define-unrepeatable-stitch mrp   (hash-ref stitch-hash #x79))
(define-unrepeatable-stitch m     (hash-ref stitch-hash #x3e))
(define-unrepeatable-stitch mp    (hash-ref stitch-hash #x40))
(define-unrepeatable-stitch kyk   (hash-ref stitch-hash #x4c))
(define-unrepeatable-stitch pyp   (hash-ref stitch-hash #x7d))
(define-unrepeatable-stitch cdi   (hash-ref stitch-hash #x69))
(define-unrepeatable-stitch mb    (hash-ref stitch-hash #xbf))
(define-unrepeatable-stitch w&t   (hash-ref stitch-hash #x50))

;; tests
(when _TEST_
  ;; keywords, single row
  (pattern #:technology 'hand #:startface 'ws #:startside 'left rows(1)(k(1)))
  ;; machine knits cannot be circular
  ;(pattern #:technology 'machine #:geometry 'circular rows(1)(k(1)))
  ;; number of stitches supplied
  (pattern rows(1 3 #:stitches 2)(k(0) m) rows(2 4)(k2tog))
  ;; constrained by stitches out
  (pattern rows(1)(k(0) m) rows(2)(k2tog))
  ;; constrained by stitches in
  (pattern rows(1)(k2tog) rows(2)(k(0) m))
  ;; constrained by both stitches in and stitches out
  (pattern rows(1 3 5)(k2tog) rows(2 4)(k(0) m))
  ;; unconstrained variable repeat
  ;(pattern row(1)(k(0)))
  ;; wrong number of stitches supplied
  ;(pattern rows(1 3 #:stitches 3)(k(0) m) rows(2 4)(k2tog))
  ;; row numbers do not start at 1
  ;(pattern rows(2 4)(k(1) m) rows(3 5)(k2tog))
  ;; non-conformable consecutive rows (caught in Row struct guard function)
  ;(pattern rows(1 2)(k(0) m) rows(3)(k2tog))
  ;; non-conformable consecutive rows (caught in Pattern struct guard function}
  ;(pattern rows(1 3)(k(1) m) rows(2 4 5)(k2tog))
  ;; non-consecutive row numbers
  ;(pattern rows(1 3)(k(1) m) rows(2 5)(k2tog))
  )
