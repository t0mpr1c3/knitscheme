#lang knit typed/racket
(require typed/racket)

;; FIXME yarn changes not implemented
;; FIXME reader/syntax tweaks not implemented (use brag?)
;; FIXME view/print methods not implemented
;; FIXME knitspeak parser not implemented
;; FIXME knitout output not implemented
;; FIXME CI not implemented
;; FIXME improve error messages/exception handling
;; FIXME need documentation

;; FIXME is there a better way than this to set compile-time variables?
(define _TEST_ #t)

(module knitstructs typed/racket
  (require typed/racket
           racket/list) ; needed for `flatten`, `range`
  (provide (all-defined-out))

  ;; define knitscheme-logger for debugging purposes
  ;; set knitscheme-receiver level to 'debug for verbose output
  (define-logger knitscheme)
  (define knitscheme-receiver (make-log-receiver knitscheme-logger 'info)) 
  (log-knitscheme-info "knitscheme-logger initialized")
  (log-knitscheme-debug "starting knit-structs module definition")

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

  ;; format row output
  (: format-rows : ((Listof Positive-Index) -> String))
  (define (format-rows xs)
    (let ([n (length xs)])
      (string-append (if (> n 1)
                         (let-values ([(middle end) (split-at-right (cdr xs) 1)])
                           (string-append (format "s ~a" (car xs))
                                          (if (> n 2)
                                              (apply string-append
                                                     (for/list : (Listof String)
                                                       ([i : Natural middle])
                                                       (format ", ~a" i)))
                                              "")
                                          (format " and ~a" (car end))))
                         (format " ~a" (car xs))))))

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
  (define-type Leaf (Pairof Natural stitch)) ; repeat-count stitch
  (define-type Node (Pairof Natural Tree)) ; repeat-count repeat-content

  ;; variable number repeat
  (: repeat : (-> (U Node Leaf) (U Node Leaf) * Node))
  (define (repeat . xs)
    (make-node 0 xs))

  ;; fixed number repeat
  (: times : (-> Natural (-> (U Node Leaf) (U Node Leaf) * Node)))
  (define ((times n) . xs)
    (make-node n xs))

  ;; aliases for small number repeats
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
      [else (error "Error in defining sequence")]))
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

  (: make-leaf : (Natural stitch -> Leaf))
  (define (make-leaf n st)
    (cons n st))

  (: leaf-count : (Leaf -> Natural))
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
  (: leaf-stitches-in : (Leaf -> Natural))
  (define (leaf-stitches-in leaf)
    (* (leaf-count leaf) (stitchtype-stitches-in (hash-ref stitch-hash (leaf-stitchtype leaf)))))

  ;; count stitches produced by leaf (excluding variable repeats)
  (: leaf-stitches-out : (Leaf -> Natural))
  (define (leaf-stitches-out leaf)
    (* (leaf-count leaf) (stitchtype-stitches-out (hash-ref stitch-hash (leaf-stitchtype leaf)))))

  ;; node functions
  (: make-node : (Natural Tree -> Node))
  (define (make-node n tree)
    (cons n tree))

  (: node-count : (Node -> Natural))
  (define (node-count node)
    (car node))

  (: node-tree : (Node -> Tree))
  (define (node-tree node)
    (cdr node))

  ;; tree functions
  (: make-tree : ((U Node Leaf) (U Node Leaf) * -> Tree))
  (define (make-tree . xs) xs)

  ;; count (non-nested) variable repeats in tree
  (: tree-count-var : (Tree -> Natural))
  (define (tree-count-var tree)
    (foldl (lambda ([x : (U Node Leaf)]
                    [y : Natural])
             (if (Leaf? x)
                 (if (zero? (leaf-count x))
                     (add1 y)
                     y)
                 (if (zero? (node-count x))
                     (add1 y)
                     ;(+ 1 y (tree-count-var (node-tree x)))
                     (+   y (tree-count-var (node-tree x))))))
           0
           tree))

  ;; obtain (leftmost non-nested) variable repeat from tree
  ;; or return #f if no variable repeat
  (: tree-var : (->* (Tree) (Natural) (U False (Pairof Tree Natural))))
  (define (tree-var tree [multiplier 1])
    (for/or : (U False (Pairof Tree Natural)) ([i (in-range (length tree))])
      (let ([x : (U Node Leaf) (list-ref tree i)])
        (if (Leaf? x)
            (if (zero? (leaf-count x))
                (cons (make-tree (make-leaf 1 (leaf-stitch x))) multiplier)
                #f)
            (if (zero? (node-count x))
                (cons (make-tree (make-node 1 (node-tree x))) multiplier)
                (tree-var (node-tree x) (* multiplier (node-count x))))))))

  ;; replace variable repeat(s) in tree with fixed integer value
  ;; if there are nested repeats, only the lowest will be replaced
  (: tree-var-replace : (Tree Natural -> Tree))
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

  ;; recursively combine consecutive leaves with same stitch type into single leaf
  ;; zero number elements are ignored
  ;; FIXME an error could be raised instead
  ;; because elsewhere 0 signifies a variable number element
  (: combine-breadth : (Tree -> Tree))
  (define (combine-breadth xs)
    (reverse
     (foldl (lambda ([x : (U Node Leaf)]
                     [y : Tree])
              (if (Leaf? x)
                  ;; leaf
                  (if (zero? (leaf-count x))                  
                      y ; ignored
                      (if (and
                           (not (null? y))
                           (Leaf? (car y))
                           (equal? (leaf-stitch x) (leaf-stitch (car y))))
                          (cons (make-leaf (+ (leaf-count x) (leaf-count (car y)))
                                           (leaf-stitch x))
                                (cdr y))
                          (cons x y)))
                  ;; node
                  (if (zero? (node-count x))                  
                      y ; ignored
                      (cons (make-node (node-count x)
                                       (combine-breadth (node-tree x))) ; match only leaves, not nodes
                            y))))            
            null
            xs)))

  ;; recursively combine singleton node/leaf nested node
  ;; assumes no zero number elements
  ;; FIXME an error could be raised instead
  (: combine-depth : (Tree -> Tree))
  (define (combine-depth xs)
    (reverse
     (foldl (lambda ([x : (U Node Leaf)]
                     [y : Tree])
              (if (Leaf? x)
                  ;; leaf
                  (cons x y)
                  ;; node
                  (if (= 1 (length (node-tree x)))
                      (let ([nested (car (node-tree x))])
                        (if (Leaf? nested)
                            ;; leaf
                            (cons (make-leaf (* (node-count x) (leaf-count nested))
                                             (leaf-stitch nested))
                                  y)
                            ;; node
                            (cons (car (combine-depth (list (make-node (* (node-count x) (node-count nested))
                                                                       (node-tree nested)))))
                                  y)))
                      (cons (make-node (node-count x)
                                       (combine-depth (node-tree x)))
                            y))))
            null
            xs)))

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

  ;; flatten tree to a list of leaves
  (: flatten-tree : (Tree -> (Listof Leaf)))
  (define (flatten-tree tree)
    (cast (combine (flatten-tree-recurse tree)) (Listof Leaf)))

  (define combine (compose1 combine-depth combine-breadth))

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
     [stitches-in-total : Natural]
     [stitches-out-total : Natural]
     [stitches-in-fixed : Natural]
     [stitches-out-fixed : Natural]
     [stitches-in-var : Natural]
     [stitches-out-var : Natural])
    #:guard
    (lambda (rownums
             stitches
             memo
             stitches-in-total
             stitches-out-total
             stitches-in-fixed
             stitches-out-fixed
             stitches-in-var
             stitches-out-var
             type-name)      
      (log-knitscheme-debug "in `Row` struct guard function")
      ;; flatten and sort list of row numbers
      (let ([rownums~ (sort ((inst uniq (Listof Positive-Index)) (flatten rownums)) <)]
            [var-count (tree-count-var stitches)]
            [flat (flatten-tree stitches)])
        ;; check valid row numbers exist
        (if (zero? (length rownums~))
            (error "no row numbers specified")
            ;; check no more than one variable repeat
            (if (> var-count 1)
                (error "more than one variable number repeat specified")
                (let ([stitches-in-fixed~ (sum (map leaf-stitches-in flat))]
                      [stitches-out-fixed~ (sum (map leaf-stitches-out flat))])
                  (if (zero? var-count)
                      ;; no variable repeats
                      (if (and
                           (not (zero? stitches-out-total))
                           (not (= stitches-out-fixed~ stitches-out-total)))
                          (error "total number of stitches produced does not match stitches in row")
                          (if (and
                               (consecutive-rows rownums~)
                               (not (= stitches-in-fixed~
                                       stitches-out-fixed~)))             
                              (error "consecutive rows are not conformable")
                              ;; result
                              (values rownums~
                                      (combine stitches)
                                      memo
                                      stitches-in-fixed~
                                      stitches-out-fixed~
                                      stitches-in-fixed~
                                      stitches-out-fixed~
                                      0
                                      0)))
                      ;; one variable repeat
                      (let* ([var (tree-var stitches)]
                             [var-multiplier (cdr var)]
                             [flat-var (flatten-tree (car var))]
                             [stitches-in-var~ (* var-multiplier (sum (map leaf-stitches-in flat-var)))]
                             [stitches-out-var~ (* var-multiplier (sum (map leaf-stitches-out flat-var)))])
                        (if (not (zero? stitches-out-total))
                            ;; #:stitches has been specified
                            ;; check that variable number repeat can be replaced by fixed value
                            (if (zero? stitches-out-var~)                     
                                (error "total number of stitches produced does not conform with variable number repeat")
                                (let-values ([(q r)
                                              (quotient/remainder (- stitches-out-total stitches-out-fixed~)
                                                                  stitches-out-var~)])
                                  (if (not (zero? r))
                                      (error "total number of stitches produced does not conform with variable number repeat")
                                      (if (consecutive-rows rownums~)
                                          ;; consecutive rows
                                          ;; replace variable number repeat by fixed value
                                          (let* ([stitches~ (tree-var-replace stitches q)]
                                                 [flat~ (flatten-tree stitches~)]
                                                 [stitches-in-total~ (sum (map leaf-stitches-in flat~))])
                                            ;; check that stitches in/out are conformable for consecutive rows
                                            (if (not (= stitches-in-total~ stitches-out-total))                 
                                                (error "consecutive rows are not conformable")
                                                ;; result
                                                (values rownums~
                                                        stitches~
                                                        memo
                                                        stitches-out-total
                                                        stitches-out-total
                                                        stitches-out-total
                                                        stitches-out-total
                                                        0
                                                        0)))
                                          ;; no consecutive rows
                                          (values rownums~
                                                  stitches
                                                  memo
                                                  stitches-in-total
                                                  stitches-out-total
                                                  stitches-in-fixed~
                                                  stitches-out-fixed~
                                                  stitches-in-var~
                                                  stitches-out-var~)))))
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
                                  (error "consecutive rows are not conformable")
                                  ;; result
                                  (values rownums~
                                          stitches
                                          memo
                                          0
                                          0
                                          stitches-in-fixed~
                                          stitches-out-fixed~
                                          stitches-in-var~
                                          stitches-out-var~)))))))))))
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
    (log-knitscheme-debug "in `rows` constructor")
    (Rows
     (cast (flatten rownums) (Listof Positive-Index))
     tree
     memo
     0 stitches-out-total 0 0 0 0))
  ;; aliases
  (define row rows)
  (define rounds rows)

  ;; information for one row/round
  ;; NB unlike Knitspeak, we do not encode Row/Round info for each row.
  ;; The whole pattern is either flat or circular.
  ;; Likewise, RS/WS information is not encoded at the row level.
  ;; RS/WS is specified only for Row 1, and alternates for flat patterns.
  ;; These are generally sensible restrictions, justifiable w.r.t. existing patterns.
  (struct Rowinfo
    ([stitches : Tree]
     [memo : String]
     [stitches-in-total : Natural]
     [stitches-out-total : Natural]
     [stitches-in-fixed : Natural]   ; only used in
     [stitches-out-fixed : Natural]  ; guard function
     [stitches-in-var : Natural]     ; and subsequently
     [stitches-out-var : Natural])   ; set to zero
    #:transparent)

  ;; 2D vector:
  ;; D1: rowinfo index
  ;; D2: vector of row numbers (1-indexed)
  (struct Rowmap
    ([data : (Immutable-Vectorof (Immutable-Vectorof Positive-Index))]
     [rowcount : Positive-Integer])
    #:guard  
    ;; check validity of row numbers
    (lambda (data rowcount type-name)
      (log-knitscheme-debug "in `Rowmap` struct guard function")  
      (let ([fl : (Listof Positive-Index) (vector->list (apply vector-append (vector->list data)))])
        (if (> (apply min fl) 1)
            (error "row numbers must start at 1")
            (if (or
                 (equal? fl '(1))
                 (equal? (uniq (diff - (sort fl <))) '(1)))
                (values data (apply max fl))
                (error "pattern must specify consecutive row numbers")))))
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
      (log-knitscheme-debug "in `Pattern` struct guard function")
      ;; circular for hand knits only
      (if (and
           (eq? technology 'machine)
           (eq? geometry 'circular))
          (error "machine knit patterns must be flat, not circular")
          ;; make vectors of stitch totals
          (let ([n-rows : Index (Rowmap-rowcount rownums)])
            (let ([stitches-in-fixed ((inst make-vector Natural) n-rows 0)]
                  [stitches-out-fixed ((inst make-vector Natural) n-rows 0)]
                  [stitches-in-var ((inst make-vector Natural) n-rows 0)]
                  [stitches-out-var ((inst make-vector Natural) n-rows 0)]
                  [stitches-in-total ((inst make-vector Natural) n-rows 0)]
                  [stitches-out-total ((inst make-vector Natural) n-rows 0)]
                  [var-count ((inst make-vector Natural) n-rows 0)]
                  [var-count-total-initial ((inst box Natural) 0)]
                  [var-count-total-final ((inst box Natural) 0)])
              (set-box! var-count-total-initial (* 1000 n-rows)) ; set to large number initially
              ;; do-loop alternates constraining adjacent rows and constraining variable repeats
              ;; until no further progress is made
              (let do-loop ()
                (log-knitscheme-debug (~a rowinfo))
                (log-knitscheme-debug (~a rownums))
                (log-knitscheme-debug (format "stitches-in-total ~a" stitches-in-total))
                (log-knitscheme-debug (format "stitches-out-total ~a" stitches-out-total))
                ;; update vectors of stitch totals
                (for ([i (in-range (vector-length (Rowmap-data rownums)))])
                  (let ([rowinfo-i (vector-ref rowinfo i)]
                        [rownums-i : (Immutable-Vectorof Index) (vector-ref (Rowmap-data rownums) i)])
                    (let ([stitches-in-total-row-i (Rowinfo-stitches-in-total rowinfo-i)]
                          [stitches-out-total-row-i (Rowinfo-stitches-out-total rowinfo-i)]  [stitches-in-fixed-row-i (Rowinfo-stitches-in-fixed rowinfo-i)]
                          [stitches-out-fixed-row-i (Rowinfo-stitches-out-fixed rowinfo-i)]
                          [stitches-in-var-row-i (Rowinfo-stitches-in-var rowinfo-i)]
                          [stitches-out-var-row-i (Rowinfo-stitches-out-var rowinfo-i)]              
                          [var-count-row-i ((compose1 tree-count-var Rowinfo-stitches) rowinfo-i)])
                      (for ([j (in-range (vector-length rownums-i))])
                        (vector-set! stitches-in-total (sub1 (vector-ref rownums-i j)) stitches-in-total-row-i)
                        (vector-set! stitches-out-total (sub1 (vector-ref rownums-i j)) stitches-out-total-row-i)
                        (vector-set! stitches-in-fixed (sub1 (vector-ref rownums-i j)) stitches-in-fixed-row-i)
                        (vector-set! stitches-out-fixed (sub1 (vector-ref rownums-i j)) stitches-out-fixed-row-i)
                        (vector-set! stitches-in-var (sub1 (vector-ref rownums-i j)) stitches-in-var-row-i)
                        (vector-set! stitches-out-var (sub1 (vector-ref rownums-i j)) stitches-out-var-row-i)
                        (vector-set! var-count (sub1 (vector-ref rownums-i j)) var-count-row-i)))))
                (set-box! var-count-total-final (sum (vector->list var-count)))
                (log-knitscheme-debug (format "var-count-total-initial ~a" (unbox var-count-total-initial)))
                (log-knitscheme-debug (format "var-count-total-final ~a" (unbox var-count-total-final)))
                ;; loop continuation expression
                (if (> (unbox var-count-total-initial)
                       (unbox var-count-total-final))
                    ;; have we just constrained some repeat variables?
                    ;; if so, do the loop again
                    (begin
                      ;; constrain adjacent rows
                      (set-box! var-count-total-initial (sum (vector->list var-count)))
                      (for ([j (in-range (sub1 n-rows))])
                        (let ([i (add1 j)])
                          (when (zero? (vector-ref stitches-in-total i))
                            (if (and
                                 (zero? (vector-ref var-count i))
                                 (not (= (vector-ref stitches-out-total j)
                                         (vector-ref stitches-in-fixed i))))
                                (error (format "pattern rows ~a and ~a are not conformable" i (add1 i)))
                                (vector-set! stitches-in-total i
                                             (vector-ref stitches-out-total j))))
                          (if (zero? (vector-ref stitches-out-total j))
                              (if (and
                                   (zero? (vector-ref var-count j))
                                   (not (= (vector-ref stitches-in-total i)
                                           (vector-ref stitches-out-fixed j))))
                                  (error (format "pattern rows ~a and ~a are not conformable" i (add1 i)))
                                  (vector-set! stitches-out-total j
                                               (vector-ref stitches-in-total i)))
                              (when (not (= (vector-ref stitches-in-total i)
                                            (vector-ref stitches-out-total j)))
                                (error (format "pattern rows ~a and ~a are not conformable" i (add1 i)))))))
                      (log-knitscheme-debug (format "stitches-in-total ~a" stitches-in-total))
                      (log-knitscheme-debug (format "stitches-out-total ~a" stitches-out-total))
                      ;; constrain variable repeats
                      (for ([j (in-range n-rows)])
                        (let ([i (add1 j)]
                              [var-count-j (vector-ref var-count j)])   
                          (when (not (zero? var-count-j))
                            (if (> var-count-j 1)
                                (error (format "more than one variable number repeat specified in row ~a" i))
                                ;; one variable repeat in row (j+1)
                                (let ([in-total (vector-ref stitches-in-total j)]
                                      [out-total (vector-ref stitches-out-total j)])                   
                                  ;; constrain variable repeat
                                  (let ([in-fixed (vector-ref stitches-in-fixed j)]
                                        [out-fixed (vector-ref stitches-out-fixed j)]
                                        [in-var (vector-ref stitches-in-var j)]
                                        [out-var (vector-ref stitches-out-var j)])
                                    (when (and
                                           (not (zero? in-total))
                                           (not (zero? out-total))
                                           (not (zero? in-var))
                                           (not (zero? out-var))
                                           (not (= (quotient (- in-total in-fixed) in-var) ; qi
                                                   (quotient (- out-total out-fixed) out-var)))) ; qo
                                      (error (format "incompatible constraints in row ~a" i)))
                                    (when (and
                                           (not (zero? in-total))
                                           (not (zero? in-var)))
                                      (let ([k (rowmap-find rownums i)])
                                        (if (not k)
                                            (error (format "could not find row ~a in pattern" i))
                                            (let* ([rowinfo-k (vector-ref rowinfo k)]
                                                   [qi (quotient (- in-total in-fixed) in-var)]
                                                   [stitches~ (tree-var-replace (Rowinfo-stitches rowinfo-k) qi)]
                                                   [out-total~ (+ out-fixed (* qi out-var))])
                                              (begin
                                                (vector-set! stitches-out-total j
                                                             out-total~)                                           
                                                (vector-set! rowinfo k
                                                             (Rowinfo
                                                              stitches~
                                                              (Rowinfo-memo rowinfo-k)
                                                              in-total
                                                              out-total~
                                                              0 0 0 0)))))))
                                    (when (and
                                           (not (zero? out-total))
                                           (not (zero? out-var)))
                                      (let ([k (rowmap-find rownums i)])
                                        (if (not k)
                                            (error (format "could not find row ~a in pattern" i))
                                            (let* ([rowinfo-k (vector-ref rowinfo k)]
                                                   [qo (quotient (- out-total out-fixed) out-var)]
                                                   [stitches~ (tree-var-replace (Rowinfo-stitches rowinfo-k) qo)]
                                                   [in-total~ (+ in-fixed (* qo in-var))])
                                              (begin
                                                (vector-set! stitches-in-total j
                                                             in-total~)
                                                (vector-set! rowinfo k
                                                             (Rowinfo
                                                              stitches~
                                                              (Rowinfo-memo rowinfo-k)
                                                              in-total~
                                                              out-total
                                                              0 0 0 0)))))))))))))
                      ;; continue loop
                      (do-loop))
                    ;; after exiting loop
                    (begin
                      ;; check for unconstrained variable repeats
                      (when (> (unbox var-count-total-final) 0)
                        (let ([xs (for/list : Natural ([j (in-range n-rows)]   
                                                       #:when (not (zero? (vector-ref var-count j))))
                                    (add1 j))])
                          ;; no constraints
                          (error (string-append "unconstrained variable repeat in row" (format-rows xs))))) 
                      ;; check that consecutive rows are conformable
                      (if (and (> n-rows 1)
                               (for/or ([i (in-range 1 n-rows)])
                                 (not (= (vector-ref stitches-in-total i) (vector-ref stitches-out-total (sub1 i))))))
                          (error "pattern rows not conformable")
                          (begin
                            ;; combine repeated stitches / nested singletons
                            ;; zero out fixed/var totals                          
                            (for ([i (in-range (vector-length (Rowmap-data rownums)))])
                              (let ([rowinfo-i (vector-ref rowinfo i)])
                                (vector-set! rowinfo i
                                             (Rowinfo
                                              (combine (Rowinfo-stitches rowinfo-i))
                                              (Rowinfo-memo rowinfo-i)
                                              (Rowinfo-stitches-in-total rowinfo-i)
                                              (Rowinfo-stitches-out-total rowinfo-i)
                                              0 0 0 0))))                    
                            ;; result
                            (values rowinfo rownums technology geometry startface startside gauge yarntype))))))))))
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
    (log-knitscheme-debug "in `pattern` constructor")
    (let ([rowinfo~
           (list->vector
            (reverse
             (foldl (lambda ([x : Rows]
                             [y : (Listof Rowinfo)])
                      (cons (Rowinfo
                             (Rows-stitches x)
                             (Rows-memo x)
                             (Rows-stitches-in-total x)
                             (Rows-stitches-out-total x)
                             (Rows-stitches-in-fixed x)
                             (Rows-stitches-out-fixed x)
                             (Rows-stitches-in-var x)
                             (Rows-stitches-out-var x))
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
      (log-knitscheme-debug (~a rows))
      (log-knitscheme-debug  (~a rowinfo~))
      (log-knitscheme-debug  (~a rowmap~))
      ;; result
      (Pattern rowinfo~ rowmap~ technology geometry startface startside gauge yarntype)))

  ;; conversion to knitspeak

  (: stitches->knitspeak : (Tree -> String))
  (define (stitches->knitspeak tree)
    (let ([str
           (foldl
            (lambda ([x : (U Node Leaf)]
                     [y : String])
              (if (Leaf? x)
                  ;; leaf
                  (let ([s (hash-ref stitch-hash (leaf-stitchtype x))]
                        [n (leaf-count x)])
                    (if (stitchtype-repeatable s)
                        (string-append y " " (stitchtype-id s) (~a n) "," )
                 
                        (string-append y " " (stitchtype-id s)
                                       (if (= n 1)
                                           ""
                                           (format " ~a times" n))
                                       ",")))
                  ;; node
                  (let ([t (node-tree x)]
                        [n (node-count x)])
                    (if (= n 1)
                        (string-append y (stitches->knitspeak t))
                        (if (= n 2)
                            (string-append y " [" (stitches->knitspeak t) " ] twice,")
                            (string-append y " [" (stitches->knitspeak t) " ] " (~a n) " times,"))))))
            ""
            tree)])
      (substring str 0 (sub1 (string-length str))))) ; remove trailing comma

  (: pattern->knitspeak : (Pattern -> String))
  (define (pattern->knitspeak p)
    (let* ([row-lex (if (eq? (Pattern-geometry p) 'circular)
                        "Round"
                        "Row")]
           [data (Rowmap-data (Pattern-rownums p))]
           ;; order rowinfos by minimum row number
           [rowinfo-pairs ((inst map (Pairof Natural Natural) Natural Natural)
                           cons
                           (map (lambda ([xs : (Vectorof Natural)]) (apply min (vector->list xs)))
                                (vector->list data))
                           (range (vector-length data)))]           
           [rowinfo-ordered (sort rowinfo-pairs
                                  (lambda ([x : (Pairof Natural Natural)] [y : (Pairof Natural Natural)])
                                    (< (car x) (car y))))]   
           [rowinfo-order ((inst map Natural (Pairof Natural Natural)) cdr rowinfo-ordered)])
      ;; loop over rowinfos
      (let loop ([i : Integer 0]
                 [res : String
                      (let ([flat : Boolean (eq? 'flat (Pattern-geometry p))]
                            [rs : Boolean (eq? 'rs (Pattern-startface p))])
                        (string-append "This "
                                       (symbol->string (Pattern-technology p))
                                       " knitting pattern is designed to be knit "
                                       (if flat
                                           "flat. Odd-numbered rows are"
                                           "in the round. Every row is")
                                       " knit on the "
                                       (if rs "RS" "WS")
                                       " of the piece"
                                       (if flat
                                           (string-append ", even-numbered rows on the " (if rs "WS" "RS"))
                                           "")
                                       ". The first row starts on the "
                                       (symbol->string (Pattern-startside p))
                                       " hand side of the pattern.\nCast on "
                                       (~a (Rowinfo-stitches-in-total (vector-ref (Pattern-rowinfo p) 0))
                                           " stitches"
                                           (if flat
                                               ""
                                               " and join in the round")
                                           ".\n")))])
        (if (< i (length rowinfo-order))
            (let* ([j (list-ref rowinfo-order i)]
                   [rownums-j (vector->list (vector-ref data j))]
                   [rowinfo-j (vector-ref (Pattern-rowinfo p) j)]
                   [memo-j (Rowinfo-memo rowinfo-j)])
              (loop (add1 i)
                    (string-append res
                                   row-lex
                                   (format-rows rownums-j)
                                   #|
                                   (if (string=? "" memo-j)
                                       ""
                                       (string-append " (memo " memo-j ")"))
                                   |# 
                                   ":"
                                   (stitches->knitspeak (Rowinfo-stitches rowinfo-j))
                                   ".\n")))
            res))))

  ;; end of module
  (log-knitscheme-debug "finishing knit-structs module definition"))

(require (submod "." knitstructs))

;; set up thread to print output from log receiver
(void 
 (thread 
  (lambda () (let sync-loop () 
               (define v (sync knitscheme-receiver))
               (printf "[~a] ~a\n" (vector-ref v 0) (vector-ref v 1)) 
               (sync-loop)))))

; macro definitions
(log-knitscheme-debug "start of macro definitions")

(define-syntax-rule (define-variable-repeat-stitch id st)
  (define-syntax (id stx)
    (syntax-case stx ()
      [(_ n) #'(make-leaf n (stitch (stitchtype-rs-symbol st) 0))]
      [_ #'(make-leaf 0 (stitch (stitchtype-rs-symbol st) 0))])))

(define-syntax-rule (define-repeatable-stitch id st)
  (define-syntax-rule (id n)
    (make-leaf n (stitch (stitchtype-rs-symbol st) 0))))

(define-syntax-rule (define-unrepeatable-stitch id st)
  (define-syntax (id stx)
    (syntax-case stx ()
      [_ #'(make-leaf 1 (stitch (stitchtype-rs-symbol st) 0))])))


; stitch function definitions  

(define-variable-repeat-stitch k    (hash-ref stitch-hash #x6b))
(define-variable-repeat-stitch p    (hash-ref stitch-hash #x70))
(define-variable-repeat-stitch ktbl (hash-ref stitch-hash #x6e))
(define-variable-repeat-stitch ptbl (hash-ref stitch-hash #x3f))
(define-variable-repeat-stitch kb   (hash-ref stitch-hash #x21))
(define-variable-repeat-stitch pb   (hash-ref stitch-hash #x25))
(define-variable-repeat-stitch bo   (hash-ref stitch-hash #x54))

(define-repeatable-stitch slwyib    (hash-ref stitch-hash #x2a))
(define-repeatable-stitch slwyif    (hash-ref stitch-hash #x51))
(define-repeatable-stitch co        (hash-ref stitch-hash #xbc))

(define-unrepeatable-stitch drop    (hash-ref stitch-hash #x2c))
(define-unrepeatable-stitch k2tog   (hash-ref stitch-hash #x55))
(define-unrepeatable-stitch p2tog   (hash-ref stitch-hash #x57))
(define-unrepeatable-stitch k3tog   (hash-ref stitch-hash #x73))
(define-unrepeatable-stitch p3tog   (hash-ref stitch-hash #x75))
(define-unrepeatable-stitch ssk     (hash-ref stitch-hash #x56))
(define-unrepeatable-stitch ssp     (hash-ref stitch-hash #x58))
(define-unrepeatable-stitch sssk    (hash-ref stitch-hash #x74))
(define-unrepeatable-stitch sssp    (hash-ref stitch-hash #x76))
(define-unrepeatable-stitch cdd     (hash-ref stitch-hash #x6f))
(define-unrepeatable-stitch yo      (hash-ref stitch-hash #x41))
(define-unrepeatable-stitch dyo     (hash-ref stitch-hash #x6a))
(define-unrepeatable-stitch ml      (hash-ref stitch-hash #x3a))
(define-unrepeatable-stitch mlp     (hash-ref stitch-hash #x78))
(define-unrepeatable-stitch mr      (hash-ref stitch-hash #x3b))
(define-unrepeatable-stitch mrp     (hash-ref stitch-hash #x79))
(define-unrepeatable-stitch m       (hash-ref stitch-hash #x3e))
(define-unrepeatable-stitch mp      (hash-ref stitch-hash #x40))
(define-unrepeatable-stitch kyk     (hash-ref stitch-hash #x4c))
(define-unrepeatable-stitch pyp     (hash-ref stitch-hash #x7d))
(define-unrepeatable-stitch cdi     (hash-ref stitch-hash #x69))
(define-unrepeatable-stitch mb      (hash-ref stitch-hash #xbf))
(define-unrepeatable-stitch w&t     (hash-ref stitch-hash #x50))

;; tests

(require typed/rackunit)

(when _TEST_

  (log-knitscheme-debug "start of tests")

  ;; tests of tree functions
  (log-knitscheme-debug "start of tests of tree functions")

  (define t1
    (make-tree (make-leaf 2 (stitch #x70 0))
               (make-node 0 (make-tree (make-leaf 1 (stitch #x6f 0))
                                       (make-node 3 (make-tree (make-leaf 2 (stitch #x54 0))))
                                       (make-leaf 2 (stitch #x6f 0))))
               (make-leaf 3 (stitch #x6a 0))))

  (check-equal?
   t1
   '((2 . #s(stitch 112 0))
     (0
      (1 . #s(stitch 111 0))
      (3 (2 . #s(stitch 84 0)))
      (2 . #s(stitch 111 0)))
     (3 . #s(stitch 106 0))))

  (check-equal?
   (flatten-tree t1)
   '((2 . #s(stitch 112 0))
     (3 . #s(stitch 106 0))))

  (check-equal?
   (tree-var-replace t1 2)   
   '((2 . #s(stitch 112 0))
     (2
      (1 . #s(stitch 111 0))
      (3 (2 . #s(stitch 84 0)))
      (2 . #s(stitch 111 0)))
     (3 . #s(stitch 106 0))))

  (check-equal?
   (tree-count-var t1)
   1)

  (check-equal?
   (tree-var t1)
   '(((1
       (1 . #s(stitch 111 0))
       (3 (2 . #s(stitch 84 0)))
       (2 . #s(stitch 111 0)))) . 1))

  (define t2
    (make-tree (make-leaf 0 (stitch #x70 0))
               (make-leaf 0 (stitch #x6f 0))))

  (check-equal?
   t2
   '((0 . #s(stitch 112 0))
     (0 . #s(stitch 111 0))))

  (check-equal?
   (flatten-tree t2)
   '())

  (check-equal?
   (tree-var-replace t2 2)   
   '((2 . #s(stitch 112 0))
     (2 . #s(stitch 111 0))))

  (check-equal?
   (tree-count-var t2)
   2)

  (check-equal?
   (tree-var t2)   
   '(((1 . #s(stitch 112 0))) . 1))

  ;; tests of `rows` constructor
  (log-knitscheme-debug "start of tests of `rows` constructor")

  ;; no variable repeats, combine adjacent leaves
  (check-equal?
   rows(1)(k(1) k(1))
   (Rows
    '(1)
    '((2 . #s(stitch 107 0))) "" 2 2 2 2 0 0))

  ;; no variable repeats, combine leaf in singleton node
  (check-equal?
   rows(1)(x2(k(1)))
   (Rows
    '(1)
    '((2 . #s(stitch 107 0))) "" 2 2 2 2 0 0))

  ;; no variable repeats, multiple simplifications
  (check-equal?
   rows(1)(x2(x3(x4(k(1) k(1)))))
   (Rows
    '(1)
    '((48 . #s(stitch 107 0))) "" 48 48 48 48 0 0))

  ;; rows not consecutive
  (check-equal?
   rows(1 3)((list-ref t1 0)
             (list-ref t1 2))
   (Rows
    '(1 3)
    '((2 . #s(stitch 112 0)) (3 . #s(stitch 106 0))) "" 11 5 11 5 0 0))

  ;; consecutive and conformable
  (check-equal?
   rows(1 2)((list-ref t1 0)
             (list-ref t1 0))
   (Rows
    '(1 2)
    '((2 . #s(stitch 112 0)) (2 . #s(stitch 112 0))) "" 4 4 4 4 0 0))

  ;; consecutive and conformable
  (check-equal?
   rows('(1 2) '(1) #:memo "new memo")((list-ref t1 0)
                                       (list-ref t1 1)
                                       (list-ref t1 2))
   (Rows
    '(1 2)
    '((2 . #s(stitch 112 0))
      (0
       (1 . #s(stitch 111 0))
       (3 (2 . #s(stitch 84 0)))
       (2 . #s(stitch 111 0)))
      (3 . #s(stitch 106 0))) "new memo"  0 0 11 5 6 3))

  ;; consecutive but not conformable   
  (check-exn
   exn:fail?
   (lambda ()
     rows(1 2)((list-ref t1 0)
               (list-ref t1 2))))

  ;; consecutive but not conformable
  (check-exn
   exn:fail?
   (lambda ()
     rows(1 2)((list-ref t1 0)
               (list-ref t1 1)
               (make-leaf 1 (stitch #x6a 0)))))

  ;; no row numbers
  (check-exn
   exn:fail?
   (lambda ()
     rows(null)((list-ref t1 0))))

  ;; tests of `pattern` constructor
  (log-knitscheme-debug "start of tests of `pattern` constructor")

  ;; keywords, single row
  (check-equal?
   (pattern #:technology 'hand #:startface 'ws #:startside 'left rows(1 #:memo "memo")(k(1)))
   (Pattern
    (vector (Rowinfo '((1 . #s(stitch 107 0))) "memo" 1 1 0 0 0 0))
    (Rowmap '#(#(1)) 1)
    'hand
    'flat
    'ws
    'left
    #f
    '#(#f)))

  ;; machine knits cannot be circular
  (check-exn
   exn:fail?
   (lambda ()
     (pattern #:technology 'machine #:geometry 'circular rows(1)(k(1)))))

  ;; variable repeat leaf, number of stitches supplied
  (check-equal?
   (pattern rows(1 3 #:stitches 2)(k(0) m) rows(2 4)(k2tog))
   (Pattern
    (vector
     (Rowinfo
      '((1 . #s(stitch 107 0))
        (1 . #s(stitch 62 0))) "" 1 2 0 0 0 0)
     (Rowinfo
      '((1 . #s(stitch 85 0))) "" 2 1 0 0 0 0))
    (Rowmap '#(#(1 3) #(2 4)) 4)
    'machine
    'flat
    'rs
    'right
    #f
    '#(#f)))

  ;; variable repeat leaf nested in node, number of stitches supplied
  (check-equal?
   (pattern rows(1 #:stitches 9)(x3(k(1) p)))
   (Pattern
    (vector
     (Rowinfo
      '((3 (1 . #s(stitch 107 0))
           (2 . #s(stitch 112 0)))) "" 9 9 0 0 0 0))
    (Rowmap '#(#(1)) 1)
    'machine
    'flat
    'rs
    'right
    #f
    '#(#f)))

  ;; variable repeat node, number of stitches supplied, variable repeat replaced in Rows struct guard function
  (check-equal?
   (pattern #:technology 'hand #:geometry 'circular rows(seq(1 4) #:stitches 8)(k(1) repeat(k(1) p(1)) k(1)))
   (Pattern
    (vector
     (Rowinfo
      '((1 . #s(stitch 107 0))
        (3 (1 . #s(stitch 107 0)) (1 . #s(stitch 112 0)))
        (1 . #s(stitch 107 0))) "" 8 8 0 0 0 0))
    (Rowmap '#(#(1 2 3 4)) 4)
    'hand
    'circular
    'rs
    'right
    #f
    '#(#f)))

  ;; variable repeat node, number of stitches supplied
  (check-equal?
   (pattern #:technology 'hand #:geometry 'flat
            rows(1 3 #:stitches 8)(k(1) repeat(k(1) p(1)) k(1))
            rows(2 4)(k(1) repeat(p(1) k(1)) k(1)))
   (Pattern
    (vector
     (Rowinfo
      '((1 . #s(stitch 107 0))
        (3 (1 . #s(stitch 107 0)) (1 . #s(stitch 112 0)))
        (1 . #s(stitch 107 0))) "" 8 8 0 0 0 0)
     (Rowinfo
      '((1 . #s(stitch 107 0))
        (3 (1 . #s(stitch 112 0)) (1 . #s(stitch 107 0)))
        (1 . #s(stitch 107 0))) "" 8 8 0 0 0 0))
    (Rowmap '#(#(1 3) #(2 4)) 4)
    'hand
    'flat
    'rs
    'right
    #f
    '#(#f)))

  ;; repeated application of constraints
  (check-equal?
   (pattern #:technology 'hand #:geometry 'flat
            rows(1 5 #:stitches 8)(k(1) repeat(k(1) p(1)) k(1))
            rows(2 6)(k(1) repeat(p(1) k(1)) k(1))
            rows(3 7)(k(1) repeat(k(2)) k(1))
            rows(4 8)(k(1) repeat(p(2)) k(1)))
   (Pattern
    (vector
     (Rowinfo
      '((1 . #s(stitch 107 0))
        (3 (1 . #s(stitch 107 0)) (1 . #s(stitch 112 0)))
        (1 . #s(stitch 107 0))) "" 8 8 0 0 0 0)
     (Rowinfo
      '((1 . #s(stitch 107 0))
        (3 (1 . #s(stitch 112 0)) (1 . #s(stitch 107 0)))
        (1 . #s(stitch 107 0))) "" 8 8 0 0 0 0)
     (Rowinfo
      '((1 . #s(stitch 107 0))
        (3 (2 . #s(stitch 107 0)))
        (1 . #s(stitch 107 0))) "" 8 8 0 0 0 0)
     (Rowinfo
      '((1 . #s(stitch 107 0))
        (3 (2 . #s(stitch 112 0)))
        (1 . #s(stitch 107 0))) "" 8 8 0 0 0 0))
    (Rowmap '#(#(1 5) #(2 6) #(3 7) #(4 8)) 8)
    'hand
    'flat
    'rs
    'right
    #f
    '#(#f)))

  ;; repeated application of constraints, last line outputs zero stitches
  (check-equal?
   (pattern #:technology 'hand #:geometry 'flat
            rows(1 5 #:stitches 8)(k(1) repeat(k(1) p(1)) k(1))
            rows(2 6)(k(1) repeat(p(1) k(1)) k(1))
            rows(3 7)(k(1) repeat(k(2)) k(1))
            rows(4 8)(k(1) repeat(p(2)) k(1))
            row(9)(bo))
   (Pattern
    (vector
     (Rowinfo
      '((1 . #s(stitch 107 0))
        (3 (1 . #s(stitch 107 0)) (1 . #s(stitch 112 0)))
        (1 . #s(stitch 107 0))) "" 8 8 0 0 0 0)
     (Rowinfo
      '((1 . #s(stitch 107 0))
        (3 (1 . #s(stitch 112 0)) (1 . #s(stitch 107 0)))
        (1 . #s(stitch 107 0))) "" 8 8 0 0 0 0)
     (Rowinfo
      '((1 . #s(stitch 107 0))
        (3 (2 . #s(stitch 107 0)))
        (1 . #s(stitch 107 0))) "" 8 8 0 0 0 0)
     (Rowinfo
      '((1 . #s(stitch 107 0))
        (3 (2 . #s(stitch 112 0)))
        (1 . #s(stitch 107 0))) "" 8 8 0 0 0 0)
     (Rowinfo '((8 . #s(stitch 84 0))) "" 8 0 0 0 0 0))
    (Rowmap '#(#(1 5) #(2 6) #(3 7) #(4 8) #(9)) 9)
    'hand
    'flat
    'rs
    'right
    #f
    '#(#f)))

  ;; constrained by stitches out
  (check-equal?
   (pattern rows(1)(k(0) m) rows(2)(k2tog))
   (Pattern
    (vector
     (Rowinfo
      '((1 . #s(stitch 107 0))
        (1 . #s(stitch 62 0))) "" 1 2 0 0 0 0)
     (Rowinfo
      '((1 . #s(stitch 85 0))) "" 2 1 0 0 0 0))
    (Rowmap '#(#(1) #(2)) 2)
    'machine
    'flat
    'rs
    'right
    #f
    '#(#f)))

  ;; constrained by stitches in
  (check-equal?
   (pattern rows(1)(k2tog) rows(2)(k(0) m))
   (Pattern
    (vector
     (Rowinfo
      '((1 . #s(stitch 85 0))) "" 2 1 0 0 0 0)
     (Rowinfo
      '((1 . #s(stitch 107 0))
        (1 . #s(stitch 62 0))) "" 1 2 0 0 0 0))
    (Rowmap '#(#(1) #(2)) 2)
    'machine
    'flat
    'rs
    'right
    #f
    '#(#f)))

  ;; constrained by both stitches in and stitches out
  (check-equal?
   (pattern rows(1 3 5)(k2tog) rows(2 4)(k(0) m))
   (Pattern
    (vector
     (Rowinfo
      '((1 . #s(stitch 85 0))) "" 2 1 0 0 0 0)
     (Rowinfo
      '((1 . #s(stitch 107 0))
        (1 . #s(stitch 62 0))) "" 1 2 0 0 0 0))
    (Rowmap '#(#(1 3 5) #(2 4)) 5)
    'machine
    'flat
    'rs
    'right
    #f
    '#(#f)))

  ;; unconstrained variable repeat (single row)
  (check-exn
   exn:fail?
   (lambda ()
     (pattern row(1)(k(0)))))

  ;; unconstrained variable repeat (two rows)
  (check-exn
   exn:fail?
   (lambda ()
     (pattern row(1)(k(0)) row(2)(p(0)))))

  ;; unconstrained variable repeat (multiple rows)
  (check-exn
   exn:fail?
   (lambda ()
     (pattern row(1)(k(0)) row(2)(k(0) p(1)) row(3)(p(0)) row(4)(p(0) k(1)))))

  ;; wrong number of stitches supplied
  (check-exn
   exn:fail?
   (lambda ()
     (pattern rows(1 3 #:stitches 3)(k(0) m) rows(2 4)(k2tog))))

  ;; row numbers do not start at 1  
  (check-exn
   exn:fail?
   (lambda ()
     (pattern rows(2 4)(k(1) m) rows(3 5)(k2tog))))

  ;; non-conformable consecutive rows (caught in Row struct guard function)
  (check-exn
   exn:fail?
   (lambda ()
     (pattern rows(1 2)(k(0) m) rows(3)(k2tog))))

  ;; non-conformable consecutive rows (caught in Pattern struct guard function}
  (check-exn
   exn:fail?
   (lambda ()
     (pattern rows(1 3)(k(1) m) rows(2 4 5)(k2tog))))

  ;; non-consecutive row numbers (caught in Rowmap struct guard function}
  (check-exn
   exn:fail?
   (lambda ()
     (pattern rows(1 3)(k(1) m) rows(2 5)(k2tog))))

  ;; more tests using times, repeat, etc.
  (log-knitscheme-debug "end of tests"))

  
(define p1
  (pattern rows(2 5)(k(1) repeat(k(1) p(1)) k(1))
           rows(1 3 #:memo "m1")(k(3) p(3))
           rows(7)(k2tog k2tog k2tog)
           rows(4 6 #:memo "m4")(x2(x3(p)))))
(log-knitscheme-info (string-append "\n" (pattern->knitspeak p1)))

(log-knitscheme-info "end of knitscheme.rkt")
;; end
