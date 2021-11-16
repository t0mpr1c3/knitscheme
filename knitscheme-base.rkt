#lang knit typed/racket
(provide (all-defined-out)
         (all-from-out 'knitscheme-base-submodule))

(require typed/racket)

;; FIXME refactor pattern etc as classes
;; FIXME knitspeak parser not implemented
;; FIXME knitout output not implemented
;; FIXME improve error messages/exception handling
;; FIXME reader/syntax tweaks not implemented (use brag?)
;; FIXME CI not implemented
;; FIXME need documentation

;; FIXME is there a better way than this to set compile-time variables?
(define _TEST_ #t)

(module knitscheme-base-submodule typed/racket
  (require typed/racket
           racket/vector ; needed for `vector-map`
           racket/list) ; needed for `flatten` `range`
  (provide (all-defined-out))

  ;; define knitscheme-logger for debugging purposes
  ;; set knitscheme-receiver level to 'debug for verbose output
  (define-logger knitscheme)
  (define knitscheme-receiver (make-log-receiver knitscheme-logger 'info))
  (log-knitscheme-info "knitscheme-logger initialized")
  (log-knitscheme-debug "starting knitscheme-base-submodule module definition")

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
    (apply + xs))

  ;; index of first non-false element of vector
  (: vector-which : ((Vectorof Any) -> (U Index False)))
  (define (vector-which vec)
    (let loop ([xs (vector->list vec)]
               [i : Natural 0])
      (let ([x (car xs)])
        (if (null? x)
            #f
            (if (not (false? x))
                (cast i Index)
                (loop (cdr xs) (add1 i)))))))

  ;; find index of largest element in list
  (: which-max : ((Listof Real) -> Index))
  (define (which-max xs)
    (argmax (λ ([i : Index]) (list-ref xs i))
            (range (length xs))))

  ;; find index of smallest element in list
  (: which-min : ((Listof Real) -> Index))
  (define (which-min xs)
    (argmin (λ ([i : Index]) (list-ref xs i))
            (range (length xs))))

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

  ;; reverse bytes
  (: bytes-reverse : (Bytes -> Bytes))
  (define (bytes-reverse b)
    (list->bytes (reverse (bytes->list b))))

  ;; stitch definitions
  ;; NB symbols are based on Stitchmastery Dash font

  ;; type of stitch
  (struct stitchtype
    ([rs-symbol : Byte]
     [ws-symbol : Byte]
     [stitches-in : Natural]
     [stitches-out : Natural]
     [offset : Integer]
     [id : String]
     [name : String]
     [rx : PRegexp]
     [repeatable : Boolean]
     [variable-repeat : Boolean])
    #:prefab)

  (define stitch-list : (Listof stitchtype)
    (list

     ;; variable repeat stitches
     (stitchtype #x6b #x70 1 1  0 "k"    "knit"       #px"k(nit ?)?_" #t #t)
     (stitchtype #x70 #x6b 1 1  0 "p"    "purl"       #px"p(url ?)?_" #t #t)
     (stitchtype #x6e #x3f 1 1  0 "ktbl" "knit-tbl"   #px"k(nit ?)?_ ?tbl" #t #t)
     (stitchtype #x3f #x6e 1 1  0 "ptbl" "purl-tbl"   #px"p(url ?)?_ ?tbl" #t #t)
     (stitchtype #x21 #x25 1 1  0 "kb"   "knit-below" #px"k(nit ?)?_ ?b(elow)?" #t #t)
     (stitchtype #x25 #x21 1 1  0 "pb"   "purl-below" #px"p(url ?)?_ ?b(elow)?" #t #t)
     (stitchtype #x54 #x54 1 0  0 "bo"   "bind-off"   #px"b(ind ?off|o) ?_" #t #t)

     ;; repeatable stitches;
     (stitchtype #x2a #x51 1 1  0 "slwyib" "slip-wyib" #px"sl(ip ?)?_ ?wyib" #t #f)
     (stitchtype #x51 #x2a 1 1  0 "slwyif" "slip-wyif" #px"sl(ip ?)?_ ?wyif" #t #f)
     (stitchtype #xbc #xbc 0 1  0 "co"     "cast-on"   #px"c(ast ?on|o) ?_" #t #f)

     ;; unrepeatable stitches
     (stitchtype #x2c #x2c 1 0  0 "dropst" "drop-stitch"              #px"drop st(itch)" #f #f)
     (stitchtype #x55 #x57 2 1  1 "k2tog"  "knit-two-together"        #px"k(nit ?)?2 ?tog(ether)?" #f #f)
     (stitchtype #x57 #x55 2 1  1 "p2tog"  "purl-two-together"        #px"p(url ?)?2 ?tog(ether)?" #f #f)
     (stitchtype #x73 #x75 3 1  2 "k3tog"  "knit-three-together"      #px"k(nit ?)?3 ?tog(ether)?" #f #f)
     (stitchtype #x75 #x73 3 1  2 "p3tog"  "purl-three-together"      #px"p(url ?)?3 ?tog(ether)?" #f #f)
     (stitchtype #x56 #x58 2 1 -1 "ssk"    "slip-slip-knit"           #px"ssk" #f #f)
     (stitchtype #x58 #x56 2 1 -1 "ssp"    "slip-slip-purl"           #px"ssp" #f #f)
     (stitchtype #x74 #x76 3 1 -2 "sssk"   "slip-slip-slip-knit"      #px"sssk" #f #f)
     (stitchtype #x76 #x74 3 1 -2 "sssp"   "slip-slip-slip-purl"      #px"sssp" #f #f)
     (stitchtype #x6a #x6a 3 1  0 "cdd"    "centered-double-decrease" #px"c((enter(ed)?|tr) d(ouble|bl) dec(rease)?|dd)" #f #f)
     (stitchtype #x6f #x6f 0 1  0 "yo"     "yarn-over"                #px"y(arn ?over|o)" #f #f)
     (stitchtype #x41 #x41 0 2  0 "dyo"    "double-yarn-over"         #px"d((ouble|bl) yarn ?over|yo)" #f #f)
     (stitchtype #x3a #x78 0 1 -1 "ml"     "make-left"                #px"m(ake ?)?_ ?l(eft)?" #f #f)
     (stitchtype #x78 #x3a 0 1 -1 "mlp"    "make-left-purlwise"       #px"m(ake ?)?_ ?l((eft)? (pw?|url(wise)?)|p)" #f #f)
     (stitchtype #x3b #x79 0 1  1 "mr"     "make-right"               #px"m(ake ?)?_ ?r(ight)?" #f #f)
     (stitchtype #x79 #x3b 0 1  1 "mrp"    "make-right-purlwise"      #px"m(ake ?)?_ ?r((ight)? (pw?|url(wise)?)|p)" #f #f)
     (stitchtype #x3e #x40 0 1  0 "m"      "make"                     #px"m(ake ?)?_" #f #f)
     (stitchtype #x40 #x3e 0 1  0 "mp"     "make-purlwise"            #px"m(ake ?)?_ ?(pw?|url(wise)?)" #f #f)
     (stitchtype #x4c #x7d 1 3  0 "kyk"    "k1-yo-k1-in-next-stitch"  #px"k(nit ?)?1,? y(arn ?over|o),? k(nit ?)?1 in(to)? next st(itch)?" #f #f)
     (stitchtype #x7d #x4c 1 3  0 "pyp"    "p1-yo-p1-in-next-stitch"  #px"p(url ?)?1,? y(arn ?over|o),? p(url ?)?1 in(to)? next st(itch)?" #f #f)
     (stitchtype #x69 #x69 1 3  0 "cdi"    "centered-double-increase" #px"c((enter(ed)?|tr) d(ouble|bl) inc(rease)?|dd)" #f #f)
     (stitchtype #xbf #xbf 1 1  0 "sp"     "special-stitch"           #px"sp(ecial( st(itch)?)?)?" #f #f) ; make bobble
     (stitchtype #x50 #x4f 0 0  0 "w&t"    "wrap-and-turn"            #px"w(rap( and |&)turn|&t)" #f #f)

     ;; other
     (stitchtype #x77 #x77 0 0  0 "ns"   "no-stitch"                 #px"(ns|no st(itch)?)" #f #f)))

  (define stitch-hash
    (for/hash : (HashTable Byte stitchtype) ([i (in-range (length stitch-list))])
      (values (stitchtype-rs-symbol (list-ref stitch-list i))
              (list-ref stitch-list i))))

  ;; stitch struct
  (struct stitch
    ([stitchtype : Byte]
     [yarntype : (Option Byte)])
    #:prefab)

  (: stitch->char : (stitch -> Char))
  (define (stitch->char st)
    (integer->char (stitch-stitchtype st)))

  ;; stitch information in each row is encoded in a tree structure:
  ;; leaf data = sequence of stitches
  ;; node data = number of repeats
  ;; each node can have multiple children

  ;; recursive definition of Tree and related types

  (define-type Tree (Listof (U Leaf Node)))
  (define-type Leaf (Pairof Natural stitch)) ; repeat-count stitch
  (define-type Node (Pairof Natural Tree)) ; repeat-count repeat-content
  (define-type Treelike (Listof (U Leaf Node Treelike)))

  ;; Treelike functions
  (define-predicate Treelike? Treelike)

  ;; convert Treelike list to Tree
  (: treelike->tree : (Treelike -> Tree))
  (define (treelike->tree xs)
    (reverse
     (foldl
      (λ ([x : (U Leaf Node Treelike)]
               [acc : Tree])
        (if (Treelike? x)
            (append (treelike->tree x) acc)
            (cons x acc)))
      null
      xs)))

  ;; variable number repeat
  (: repeat : (-> (U Leaf Node Treelike) (U Leaf Node Treelike) * Node))
  (define (repeat . xs)
    (make-node 0 (treelike->tree xs)))

  ;; fixed number repeat
  (: times : (-> Natural (-> (U Leaf Node Treelike) (U Leaf Node Treelike) * Node)))
  (define ((times n) . xs)
    (make-node n (treelike->tree xs)))

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
      [(_) (let ([xn-id (λ (i) (format-id stx "x~a" i))])
             (with-syntax ([((n xn) ...) (map (λ (j) (list j (xn-id j))) (range 1 21))])
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

  ;; end of macro definitions


  ;; leaf functions
  (define-predicate Leaf? Leaf)

  ;; constructor
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

  (: leaf-yarntype : (Leaf -> (Option Byte)))
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
  (define-predicate Node? Node)

  ;; constructor
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
  (define-predicate Tree? Tree)

  ;; constructor
  (: make-tree : ((U Leaf Node) (U Leaf Node) * -> Tree))
  (define (make-tree . xs) xs)

  ;; count (non-nested) variable repeats in tree
  (: tree-count-var : (Tree -> Natural))
  (define (tree-count-var tree)
    (foldl (λ ([x : (U Leaf Node)]
                    [acc : Natural])
             (if (Leaf? x)
                 ;; leaf
                 (if (zero? (leaf-count x))
                     (add1 acc)
                     acc)
                 ;; node
                 (if (zero? (node-count x))
                     (add1 acc)
                     (+ acc (tree-count-var (node-tree x))))))
           0
           tree))

  ;; obtain (leftmost non-nested) variable repeat from tree
  ;; or return #f if no variable repeat
  (: tree-var : (->* (Tree) (Natural) (Option (Pairof Tree Natural))))
  (define (tree-var tree [multiplier 1])
    (for/or : (Option (Pairof Tree Natural)) ([i (in-range (length tree))])
      (let ([x : (U Leaf Node) (list-ref tree i)])
        (if (Leaf? x)
            ;; leaf
            (if (zero? (leaf-count x))
                (cons (make-tree (make-leaf 1 (leaf-stitch x))) multiplier)
                #f)
            ;; node
            (if (zero? (node-count x))
                (cons (make-tree (make-node 1 (node-tree x))) multiplier)
                (tree-var (node-tree x) (* multiplier (node-count x))))))))

  ;; replace variable repeat(s) in tree with fixed integer value
  ;; if there are nested repeats, only the lowest will be replaced
  (: tree-var-replace : (Tree Natural -> Tree))
  (define (tree-var-replace tree r) : Tree
    (reverse
     (foldl (λ ([x : (U Leaf Node)]
                     [acc : Tree])
              (if (Leaf? x)
                  ;; leaf
                  (if (zero? (leaf-count x))
                      (cons (make-leaf r (leaf-stitch x)) acc)
                      (cons x acc))
                  ;; node
                  (if (zero? (node-count x))
                      (cons (make-node r (node-tree x)) acc)
                      (cons (make-node (node-count x) (tree-var-replace (node-tree x) r)) acc))))
            null
            tree)))

  ;; recursively combine first by breadth, then by depth, until there are no further changes
  (: combine : (Tree -> Tree))
  (define (combine xs)
    (let ([xs~ (combine-depth (combine-breadth xs))])
      (if (equal? xs xs~)
          xs~
          (combine xs~))))

  ;; recursively combine consecutive leaves with same stitch type into single leaf
  ;; eliminate zero number elements
  (: combine-breadth : (Tree -> Tree))
  (define (combine-breadth xs)
    (reverse
     (foldl (λ ([x : (U Leaf Node)]
                     [acc : Tree])
              (if (Leaf? x)
                  ;; leaf
                  (let ([n (leaf-count x)])
                    (if (zero? n)
                        acc ; eliminated
                        (if (null? acc)
                            (cons x acc)
                            (let ([h (car acc)])
                              (if (not (Leaf? h))
                                  (cons x acc)
                                  (let ([s (leaf-stitch x)])
                                    (if (equal? s (leaf-stitch h))
                                        (cons (make-leaf (+ n (leaf-count h)) s)
                                              (cdr acc))
                                        (cons x acc))))))))
                  ;; node
                  (let ([n (node-count x)])
                    (if (zero? n)
                        acc ; eliminated
                        (cons (make-node n (combine-breadth (node-tree x))) ; match only leaves, not nodes
                              acc)))))
            null
            xs)))

  ;; recursively combine singleton node/leaf nested node
  ;; eliminate zero number elements
  (: combine-depth : (Tree -> Tree))
  (define (combine-depth xs)
    (reverse
     (foldl (λ ([x : (U Leaf Node)]
                     [acc : Tree])
              (if (Leaf? x)
                  ;; leaf
                  (cons x acc)
                  ;; node
                  (let node-loop : Tree ([n : Natural (node-count x)]
                                         [t (node-tree x)])
                    (if (= 1 (length t))
                        ;; singleton node
                        (let ([x~ (car t)])
                          (if (Leaf? x~)
                              ;; leaf
                              (let ([n~ (leaf-count x~)])
                                (if (zero? n~)
                                    acc ; eliminated
                                    (cons (make-leaf (* n n~)
                                                     (leaf-stitch x~))
                                          acc)))
                              ;; node
                              (let ([n~ (node-count x~)])
                                (if (zero? n~)
                                    acc ; eliminated
                                    (node-loop (* n n~)
                                               (node-tree x~))))))
                        ;; not singleton node
                        (cons (make-node n (combine-depth t))
                              acc)))))
            null
            xs)))

  ;; flatten every node in tree to list of leaves
  ;; ignores variable repeats
  (: flatten-tree-recurse : (Tree -> Tree))
  (define (flatten-tree-recurse tree)
    (foldl (λ ([x : (U Leaf Node)]
                    [acc : Tree])
             (if (Leaf? x)
                 ;; leaf
                 (cons x acc)
                 ;; node
                 (let loop : Tree
                   ([t : Tree (flatten-tree-recurse (node-tree x))]
                    [n : Natural (node-count x)]
                    [res : Tree acc])
                   (if (> n 0)
                       (loop t (sub1 n) (append t res))
                       res))))
           null
           tree))

  ;; flatten tree to a list of leaves
  (: flatten-tree : (Tree -> (Listof Leaf)))
  (define (flatten-tree tree)
    (cast (combine (reverse (flatten-tree-recurse tree))) (Listof Leaf)))

  ;; define data structs

  ;; non-empty list

  ;; yarn
  (struct yarntype
    ([brand : String]
     [fiber : String]
     [weight : String]
     [color : String])
    #:prefab)

  ;; yarn functions

  ;; constructor
  (: make-yarn : (->* () (String String String String) yarntype))
  (define (make-yarn [color : String ""]
                     [weight : String ""]
                     [fiber : String ""]
                     [brand : String ""])
    (yarntype color weight fiber brand))

  (: yarns : ((Listof String) (Listof String) * -> (Immutable-Vectorof yarntype)))
  (define (yarns . ys)
    (vector->immutable-vector
     (list->vector
      (map (λ ([x : (Listof String)])
             (match x
               [(list a b c d e ...) (make-yarn a b c d)] ; discard additional strings
               [(list a b c)         (make-yarn a b c)]
               [(list a b)           (make-yarn a b)]
               [(list a)             (make-yarn a)]
               [null                 (make-yarn)]))
           ys))))

  (: with-yarn : (Byte -> Symbol))
  (define (with-yarn n)
    (if (zero? n)
        'mc
        (string->symbol (format "cc~a" n))))

  (: yarn : (-> (Option Byte) (-> (U Leaf Node Treelike) (U Leaf Node Treelike) * Tree)))
  (define ((yarn n) . xs)
    (yarn-recurse n (treelike->tree xs)))

  (: yarn-recurse : ((Option Byte) Tree -> Tree))
  (define (yarn-recurse n xs)
    (foldl (λ ([x : (U Leaf Node)]
                    [acc : Tree])
             (if (Leaf? x)
                 ;; leaf
                 (if (or
                      (false? n)
                      (false? (leaf-yarntype x)))
                     (cons (make-leaf (leaf-count x) (stitch (leaf-stitchtype x) n))
                           acc)
                     (cons x acc))
                 ;; node
                 (cons (make-node (node-count x) (reverse (yarn-recurse n (node-tree x))))
                       acc)))
           null
           xs))

  (define mc (yarn 0))

  ;; macro to define yarn functions cc1 ... cc20
  (define-syntax (define-ccns stx)
    (syntax-case stx ()
      [(_) (let ([ccn-id (λ (i) (format-id stx "cc~a" i))])
             (with-syntax ([((n ccn) ...) (map (λ (j) (list j (ccn-id j))) (range 1 21))])
               #'(begin
                   (define ccn (yarn n))
                   ...)))]))
  (define-ccns)

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
     [yarn : (Option Byte)]
     [stitches-in-total : Natural]
     [stitches-out-total : Natural]
     [stitches-in-fixed : Natural]
     [stitches-out-fixed : Natural]
     [stitches-in-var : Natural]
     [stitches-out-var : Natural])
    #:guard
    (λ (rownums
             stitches
             memo
             yarn
             stitches-in-total
             stitches-out-total
             stitches-in-fixed
             stitches-out-fixed
             stitches-in-var
             stitches-out-var
             type-name)
      (log-knitscheme-debug "in `Row` struct guard function")
      ;; NB composed functions are applied in reverse order
      ((compose rows-guard-dispatch
                rows-guard-rownums)
       rownums
       stitches
       memo
       yarn
       stitches-in-total
       stitches-out-total
       stitches-in-fixed
       stitches-out-fixed
       stitches-in-var
       stitches-out-var))
    #:transparent)

  ;; composable function as part of `Rows` struct guard function
  (: rows-guard-rownums : ((Listof Positive-Index)
                           Tree
                           String
                           (Option Byte)
                           Natural
                           Natural
                           Natural
                           Natural
                           Natural
                           Natural
                           -> (values (Listof Positive-Index)
                                      Tree
                                      String
                                      (Option Byte)
                                      Natural
                                      Natural
                                      Natural
                                      Natural
                                      Natural
                                      Natural)))
  (define (rows-guard-rownums rownums
                              stitches
                              memo
                              yarn
                              stitches-in-total
                              stitches-out-total
                              stitches-in-fixed
                              stitches-out-fixed
                              stitches-in-var
                              stitches-out-var)
    ;; check valid row numbers exist
    (let ([rownums~ (sort (uniq rownums) <)])
      (if (zero? (length rownums~))
          (error "no row numbers specified")
          (values rownums~
                  stitches
                  memo
                  yarn
                  stitches-in-total
                  stitches-out-total
                  stitches-in-fixed
                  stitches-out-fixed
                  stitches-in-var
                  stitches-out-var))))

  ;; composable function as part of `Rows` struct guard function
  (: rows-guard-dispatch : ((Listof Positive-Index)
                            Tree
                            String
                            (Option Byte)
                            Natural
                            Natural
                            Natural
                            Natural
                            Natural
                            Natural
                            -> (values (Listof Positive-Index)
                                       Tree
                                       String
                                       (Option Byte)
                                       Natural
                                       Natural
                                       Natural
                                       Natural
                                       Natural
                                       Natural)))
  (define (rows-guard-dispatch rownums
                               stitches
                               memo
                               yarn
                               stitches-in-total
                               stitches-out-total
                               stitches-in-fixed
                               stitches-out-fixed
                               stitches-in-var
                               stitches-out-var)
    ;; flatten and sort list of row numbers
    (let ([var-count (tree-count-var stitches)])
      ;; check no more than one variable repeat
      (if (> var-count 1)
          (error "more than one variable number repeat specified")
          (if (zero? var-count)
              ;; no variable repeats
              (rows-guard-fixed rownums
                                stitches
                                memo
                                yarn
                                stitches-in-total
                                stitches-out-total
                                stitches-in-fixed
                                stitches-out-fixed
                                stitches-in-var
                                stitches-out-var)
              ;; one variable repeat
              (if (not (zero? stitches-out-total))
                  ;; #:stitches has been specified
                  (rows-guard-var-replace rownums
                                          stitches
                                          memo
                                          yarn
                                          stitches-in-total
                                          stitches-out-total
                                          stitches-in-fixed
                                          stitches-out-fixed
                                          stitches-in-var
                                          stitches-out-var)
                  ;; #:stitches has not been specified
                  (rows-guard-var-keep rownums
                                       stitches
                                       memo
                                       yarn
                                       stitches-in-total
                                       stitches-out-total
                                       stitches-in-fixed
                                       stitches-out-fixed
                                       stitches-in-var
                                       stitches-out-var))))))

  ;; composable function as part of `Rows` struct guard function
  (: rows-guard-fixed : ((Listof Positive-Index)
                         Tree
                         String
                         (Option Byte)
                         Natural
                         Natural
                         Natural
                         Natural
                         Natural
                         Natural
                         -> (values (Listof Positive-Index)
                                    Tree
                                    String
                                    (Option Byte)
                                    Natural
                                    Natural
                                    Natural
                                    Natural
                                    Natural
                                    Natural)))
  (define (rows-guard-fixed rownums
                            stitches
                            memo
                            yarn
                            stitches-in-total
                            stitches-out-total
                            stitches-in-fixed
                            stitches-out-fixed
                            stitches-in-var
                            stitches-out-var)
    (let* ([flat (flatten-tree stitches)]
           [stitches-out-fixed~ (cast (sum (map leaf-stitches-out flat)) Natural)])
      (if (and
           (not (zero? stitches-out-total))
           (not (= stitches-out-fixed~
                   stitches-out-total)))
          (error "number of stitches produced in row does not match specified stitch count")
          (let ([stitches-in-fixed~ (cast (sum (map leaf-stitches-in flat)) Natural)])
            (if (and
                 (consecutive-rows rownums)
                 (not (= stitches-in-fixed~
                         stitches-out-fixed~)))
                (error "consecutive rows do not have conformable stitch counts")
                ;; result
                (values rownums
                        (combine stitches)
                        memo
                        yarn
                        stitches-in-fixed~
                        stitches-out-fixed~
                        stitches-in-fixed~
                        stitches-out-fixed~
                        0
                        0))))))

  ;; composable function as part of `Rows` struct guard function
  (: rows-guard-var-replace : ((Listof Positive-Index)
                               Tree
                               String
                               (Option Byte)
                               Natural
                               Natural
                               Natural
                               Natural
                               Natural
                               Natural
                               -> (values (Listof Positive-Index)
                                          Tree
                                          String
                                          (Option Byte)
                                          Natural
                                          Natural
                                          Natural
                                          Natural
                                          Natural
                                          Natural)))
  (define (rows-guard-var-replace rownums
                                  stitches
                                  memo
                                  yarn
                                  stitches-in-total
                                  stitches-out-total
                                  stitches-in-fixed
                                  stitches-out-fixed
                                  stitches-in-var
                                  stitches-out-var)
    (let ([var (tree-var stitches)])
      (if (false? var)
          (error "this should never happen")
          ;; check that variable number repeat can be replaced by fixed value
          (let* ([var-multiplier ((inst cdr Tree Natural) var)]
                 [flat-var (flatten-tree (car var))]
                 [stitches-out-var~ (cast (* var-multiplier (sum (map leaf-stitches-out flat-var))) Natural)])
            (if (zero? stitches-out-var~)
                (error "number of stitches produced in row does not conform with variable number repeat")
                (let* ([flat (flatten-tree stitches)]
                       [stitches-out-fixed~ (cast (sum (map leaf-stitches-out flat)) Natural)])
                  (let-values ([(q r)
                                (quotient/remainder (- stitches-out-total stitches-out-fixed~)
                                                    stitches-out-var~)])
                    (if (or
                         (negative? q)
                         (not (zero? r)))
                        (error "number of stitches produced in row does not conform with variable number repeat")
                        ;; replace variable number repeat by fixed value
                        (let* ([stitches~ (tree-var-replace stitches q)]
                               [flat~ (flatten-tree stitches~)]
                               [stitches-in-total~ (cast (sum (map leaf-stitches-in flat~)) Natural)])
                          (if (consecutive-rows rownums)
                              ;; consecutive rows
                              ;; check that stitches in/out are conformable for consecutive rows
                              (if (not (= stitches-in-total~
                                          stitches-out-total))
                                  (error "consecutive rows do not have conformable stitch counts")
                                  ;; result
                                  (values rownums
                                          (combine stitches~)
                                          memo
                                          yarn
                                          stitches-out-total
                                          stitches-out-total
                                          stitches-out-total
                                          stitches-out-total
                                          0
                                          0))
                              ;; no consecutive rows
                              (let ([stitches-in-fixed~ (cast (sum (map leaf-stitches-in flat)) Natural)]
                                    [stitches-in-var~ (cast (* var-multiplier (sum (map leaf-stitches-in flat-var))) Natural)])
                                (values rownums
                                        (combine stitches~)
                                        memo
                                        yarn
                                        stitches-in-total~
                                        stitches-out-total
                                        stitches-in-fixed~
                                        stitches-out-fixed~
                                        (if (zero? q) 0 stitches-in-var~)
                                        (if (zero? q) 0 stitches-out-var~)))))))))))))
    
  ;; composable function as part of `Rows` struct guard function
  (: rows-guard-var-keep : ((Listof Positive-Index)
                            Tree
                            String
                            (Option Byte)
                            Natural
                            Natural
                            Natural
                            Natural
                            Natural
                            Natural
                            -> (values (Listof Positive-Index)
                                       Tree
                                       String
                                       (Option Byte)
                                       Natural
                                       Natural
                                       Natural
                                       Natural
                                       Natural
                                       Natural)))
  (define (rows-guard-var-keep rownums
                               stitches
                               memo
                               yarn
                               stitches-in-total
                               stitches-out-total
                               stitches-in-fixed
                               stitches-out-fixed
                               stitches-in-var
                               stitches-out-var)
    (let ([var (tree-var stitches)])
      (if (false? var)
          (error "this should never happen")
          (let* ([var-multiplier ((inst cdr Tree Natural) var)]
                 [flat-var (flatten-tree (car var))]
                 [stitches-in-var~ (cast (* var-multiplier (sum (map leaf-stitches-in flat-var))) Natural)]
                 [stitches-out-var~ (cast (* var-multiplier (sum (map leaf-stitches-out flat-var))) Natural)]
                 [flat (flatten-tree stitches)]
                 [stitches-out-fixed~ (cast (sum (map leaf-stitches-out flat)) Natural)]
                 [stitches-in-fixed~ (cast (sum (map leaf-stitches-in flat)) Natural)]
                 [diff-fixed (abs (- stitches-in-fixed~ stitches-out-fixed~))]
                 [diff-var (abs (- stitches-in-var~ stitches-out-var~))])
            ;; check for consecutive rows
            (if (and
                 (consecutive-rows rownums)
                 ;; check that stitches in/out are conformable for consecutive rows
                 ;; conformable means that the difference between the stitches in/out
                 ;; is an integer multiple of the difference between the variable stitches in/out
                 (not (zero? diff-fixed))
                 (or (zero? diff-var)
                     (not (zero? (remainder diff-fixed diff-var)))))
                (error "consecutive rows do not have conformable stitch counts") ; FIXME identify bad errors in error message
                ;; result
                (values rownums
                        stitches
                        memo
                        yarn
                        0
                        0
                        stitches-in-fixed~
                        stitches-out-fixed~
                        stitches-in-var~
                        stitches-out-var~))))))

  #|
  ;; FIXME need to define macro to insert memo keyword
  (: memo (-> String Symbol))
  (define (memo m) (string->symbol m))
  |#

  ;; Rownums type
  (define-type Rownums-input (Listof (U Positive-Index Rownums-input)))

  ;; alternative constructor
  (: rows : (->* () (#:memo String #:stitches Natural #:yarn (Option Byte))
                 #:rest (U Positive-Index Rownums-input)
                 (-> (U Leaf Node Treelike) (U Leaf Node Treelike) * Rows)))
  (define ((rows #:memo [memo : String ""]
                 #:stitches [stitches-out-total : Natural 0]
                 #:yarn [y : (Option Byte) 0] ; MC is default yarn
                 . rownums) . xs)
    (log-knitscheme-debug "in `rows` constructor")
    (Rows
     ;; flatten rownums, not concerned about order
     (let flatten ([xs : Rownums-input rownums]
                   [acc : (Listof Positive-Index) null])
       (foldl (λ ([x : (U Positive-Index Rownums-input)]
                       [acc : (Listof Positive-Index)])
                (if (number? x)
                    (cons x acc)
                    (append (flatten x null) acc)))
              null
              xs))
     (yarn y)(xs)
     memo
     y
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
     [yarn : (Option Byte)]
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
    (λ (data rowcount type-name)
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
      (λ ([rs : (Vectorof Positive-Index)]) (vector-memq r rs))
      (Rowmap-data rowmap))))

  ;; gauge
  (struct Gauge
    ([st-count-x : Positive-Float]
     [st-count-y : Positive-Float]
     [measurement-x : Positive-Float]
     [measurement-y : Positive-Float]
     [unit : (U 'cm 'inch)])
    #:prefab)

  ;; Pattern struct
  (struct Pattern
    ([name : String]
     [rowinfos : (Vectorof Rowinfo)]
     [rowmap : Rowmap]
     [technology : (U 'hand 'machine)]
     [geometry : (U 'flat 'circular)]
     [startface : (U 'rs 'ws)]
     [startside : (U 'left 'right)]
     [gauge : (Option Gauge)]
     [yarns : (Immutable-Vectorof (Option yarntype))])
    #:guard
    (λ (name rowinfos rowmap technology geometry startface startside gauge yarns type-name)
      (log-knitscheme-debug "in `Pattern` struct guard function")
      ;; NB composed functions are applied in reverse order
      ((compose pattern-guard-combine-stitches
                pattern-guard-rows-conformable
                pattern-guard-unconstrained-var
                pattern-guard-stitch-counts
                pattern-guard-yarns
                pattern-guard-options)
       name rowinfos rowmap technology geometry startface startside gauge yarns))
    #:transparent)

  ;; composable guard function for Pattern struct
  (: pattern-guard-options : (String
                              (Vectorof Rowinfo)
                              Rowmap
                              (U 'hand 'machine)
                              (U 'flat 'circular)
                              (U 'rs 'ws)
                              (U 'left 'right)
                              (Option Gauge)
                              (Immutable-Vectorof (Option yarntype)) ->
                              (values String
                                      (Vectorof Rowinfo)
                                      Rowmap
                                      (U 'hand 'machine)
                                      (U 'flat 'circular)
                                      (U 'rs 'ws)
                                      (U 'left 'right)
                                      (Option Gauge)
                                      (Immutable-Vectorof (Option yarntype)))))
  (define (pattern-guard-options name rowinfos rowmap technology geometry startface startside gauge yarns)
    ;; circular knitting is for hand knits only, not machine knits
    (if (and
         (eq? technology 'machine)
         (eq? geometry 'circular))
        (error "machine knit patterns must be flat, not circular")
        (values name rowinfos rowmap technology geometry startface startside gauge yarns)))

  ;; composable guard function for Pattern struct
  (: pattern-guard-yarns : (String
                            (Vectorof Rowinfo)
                            Rowmap
                            (U 'hand 'machine)
                            (U 'flat 'circular)
                            (U 'rs 'ws)
                            (U 'left 'right)
                            (Option Gauge)
                            (Immutable-Vectorof (Option yarntype)) ->
                            (values String
                                    (Vectorof Rowinfo)
                                    Rowmap
                                    (U 'hand 'machine)
                                    (U 'flat 'circular)
                                    (U 'rs 'ws)
                                    (U 'left 'right)
                                    (Option Gauge)
                                    (Immutable-Vectorof (Option yarntype)))))
  (define (pattern-guard-yarns name rowinfos rowmap technology geometry startface startside gauge yarns)
    ;; maximum number of yarns that can be specified is 256
    (if (> (vector-length yarns) 256)
        (error "too many yarns specified")
        (values name rowinfos rowmap technology geometry startface startside gauge yarns)))

  ;; composable guard function for Pattern struct
  (: pattern-guard-stitch-counts : (String
                                    (Vectorof Rowinfo)
                                    Rowmap
                                    (U 'hand 'machine)
                                    (U 'flat 'circular)
                                    (U 'rs 'ws)
                                    (U 'left 'right)
                                    (Option Gauge)
                                    (Immutable-Vectorof (Option yarntype)) ->
                                    (values String
                                            (Vectorof Rowinfo)
                                            Rowmap
                                            (U 'hand 'machine)
                                            (U 'flat 'circular)
                                            (U 'rs 'ws)
                                            (U 'left 'right)
                                            (Option Gauge)
                                            (Immutable-Vectorof (Option yarntype)))))
  (define (pattern-guard-stitch-counts name rowinfos rowmap technology geometry startface startside gauge yarns)
    (let* ([n-rows : Natural (Rowmap-rowcount rowmap)]
           [var-count-total-initial : Natural (* 1000 n-rows)]) ; set to large number initially
      (pattern-guard-stitch-counts-recurse
       var-count-total-initial name rowinfos rowmap technology geometry startface startside gauge yarns)))

  ;; FIXME remove imperative code
  (: pattern-guard-stitch-counts-recurse : (Natural
                                            String
                                            (Vectorof Rowinfo)
                                            Rowmap
                                            (U 'hand 'machine)
                                            (U 'flat 'circular)
                                            (U 'rs 'ws)
                                            (U 'left 'right)
                                            (Option Gauge)
                                            (Immutable-Vectorof (Option yarntype)) ->
                                            (values String
                                                    (Vectorof Rowinfo)
                                                    Rowmap
                                                    (U 'hand 'machine)
                                                    (U 'flat 'circular)
                                                    (U 'rs 'ws)
                                                    (U 'left 'right)
                                                    (Option Gauge)
                                                    (Immutable-Vectorof (Option yarntype)))))
  (define (pattern-guard-stitch-counts-recurse
           var-count-total-initial name rowinfos rowmap technology geometry startface startside gauge yarns)
    (log-knitscheme-debug "in function `pattern-guard-stitch-counts-recurse`")
    ;; recalculate number of variable repeats
    (let* ([n-rows : Natural (Rowmap-rowcount rowmap)]
           [var-count ((inst make-vector Natural) n-rows 0)])
      (for ([i (in-range (vector-length (Rowmap-data rowmap)))])
        (let* ([rowinfo-i (vector-ref rowinfos i)]
               [rownums-i : (Immutable-Vectorof Index) (vector-ref (Rowmap-data rowmap) i)]
               [var-count-row-i (tree-count-var (Rowinfo-stitches rowinfo-i))])
          (for ([j (in-range (vector-length rownums-i))])
            (vector-set! var-count (sub1 (vector-ref rownums-i j)) var-count-row-i))))
      (let ([var-count-total-final (cast (sum (vector->list var-count)) Natural)])
        (log-knitscheme-debug (~a rowinfos))
        (log-knitscheme-debug (~a rowmap))
        (log-knitscheme-debug (format "var-count-total-initial ~a" var-count-total-initial))
        (log-knitscheme-debug (format "var-count-total-final ~a" var-count-total-final))
        ;; recursion continuation expression
        (if (> var-count-total-initial
               var-count-total-final)
            ;; have we just constrained some repeat variables?
            ;; if so, continue and recurse
            (begin
              ;; make vectors of stitch totals
              (let ([stitches-in-fixed ((inst make-vector Natural) n-rows 0)]
                    [stitches-out-fixed ((inst make-vector Natural) n-rows 0)]
                    [stitches-in-var ((inst make-vector Natural) n-rows 0)]
                    [stitches-out-var ((inst make-vector Natural) n-rows 0)]
                    [stitches-in-total ((inst make-vector Natural) n-rows 0)]
                    [stitches-out-total ((inst make-vector Natural) n-rows 0)])
                (for ([i (in-range (vector-length (Rowmap-data rowmap)))])
                  (let* ([rowinfo-i (vector-ref rowinfos i)]
                         [rownums-i : (Immutable-Vectorof Index) (vector-ref (Rowmap-data rowmap) i)]
                         [stitches-in-total-row-i (Rowinfo-stitches-in-total rowinfo-i)]
                         [stitches-out-total-row-i (Rowinfo-stitches-out-total rowinfo-i)]
                         [stitches-in-fixed-row-i (Rowinfo-stitches-in-fixed rowinfo-i)]
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
                      (vector-set! var-count (sub1 (vector-ref rownums-i j)) var-count-row-i))))
                (log-knitscheme-debug (format "stitches-in-total ~a" stitches-in-total))
                (log-knitscheme-debug (format "stitches-out-total ~a" stitches-out-total))
                ;; constrain adjacent rows
                ;; loop over row numbers
                (let j-loop ([j : Index 0])
                  (when (< j (sub1 n-rows))
                    (let ([i (cast (add1 j) Positive-Index)]) ; 1-indexed row number
                      (when (zero? (vector-ref stitches-in-total i))
                        (if (and
                             (zero? (vector-ref var-count i))
                             (not (= (vector-ref stitches-out-total j)
                                     (vector-ref stitches-in-fixed i))))
                            (error (format "pattern rows ~a and ~a do not have conformable stitch counts" i (add1 i)))
                            (vector-set! stitches-in-total i
                                         (vector-ref stitches-out-total j))))
                      (if (zero? (vector-ref stitches-out-total j))
                          (if (and
                               (zero? (vector-ref var-count j))
                               (not (= (vector-ref stitches-in-total i)
                                       (vector-ref stitches-out-fixed j))))
                              (error (format "pattern rows ~a and ~a do not have conformable stitch counts" i (add1 i)))
                              (vector-set! stitches-out-total j
                                           (vector-ref stitches-in-total i)))
                          (when (not (= (vector-ref stitches-in-total i)
                                        (vector-ref stitches-out-total j)))
                            (error (format "pattern rows ~a and ~a do not have conformable stitch counts" i (add1 i)))))
                      (j-loop i))))
                (log-knitscheme-debug "after constraining adjacent rows:")
                (log-knitscheme-debug (format "stitches-in-total ~a" stitches-in-total))
                (log-knitscheme-debug (format "stitches-out-total ~a" stitches-out-total))
                ;; constrain variable repeats
                ;; loop over row numbers
                (let j-loop ([j : Index 0])
                  (when (< j n-rows)
                    (let ([i (cast (add1 j) Positive-Index)] ; 1-indexed row number
                          [var-count-j (vector-ref var-count j)])
                      (when (not (zero? var-count-j))
                        ;; at least one variable repeat in row i
                        (if (> var-count-j 1)
                            (error (format "pattern row ~a has more than one variable number repeat specified" i))
                            ;; one variable repeat in row i
                            (let ([in-total (vector-ref stitches-in-total j)]
                                  [out-total (vector-ref stitches-out-total j)]
                                  [in-fixed (vector-ref stitches-in-fixed j)]
                                  [out-fixed (vector-ref stitches-out-fixed j)]
                                  [in-var (vector-ref stitches-in-var j)]
                                  [out-var (vector-ref stitches-out-var j)])
                              ;; check for incompatible constraints
                              (when (and
                                     (not (zero? in-total))
                                     (not (zero? out-total))
                                     (not (zero? in-var))
                                     (not (zero? out-var))
                                     (not (= (quotient (- in-total in-fixed) in-var) ; qi
                                             (quotient (- out-total out-fixed) out-var)))) ; qo
                                (error (format "pattern row ~a has incompatible stitch number constraints" i)))
                              ;; check for constraint on stitches consumed
                              (when (and
                                     (not (zero? in-total))
                                     (not (zero? in-var)))
                                ;; constraint exists
                                ;; find rowinfo index k that corresponds to row number i
                                (let ([k (rowmap-find rowmap i)])
                                  (if (not k)
                                      (error (format "could not find row ~a in pattern" i))
                                      (let ([rowinfo-k (vector-ref rowinfos k)]
                                            [qi (quotient (- in-total in-fixed) in-var)])
                                        (if (negative? qi)
                                            (error (format "pattern row ~a has negative value of variable repeat" i))
                                            (let ([stitches~ (tree-var-replace (Rowinfo-stitches rowinfo-k) qi)]
                                                  [out-total~ (+ out-fixed (* qi out-var))])
                                              (begin
                                                ;((inst vector-set! Natural) stitches-out-total j out-total~)
                                                (vector-set! rowinfos k
                                                             (Rowinfo
                                                              (if (zero? qi)
                                                                  (combine stitches~) ; eliminate variable repeat evaluated at zero
                                                                  stitches~)
                                                              (Rowinfo-memo rowinfo-k)
                                                              (Rowinfo-yarn rowinfo-k)
                                                              in-total
                                                              out-total~
                                                              0 0 0 0)))))))))
                              ;; check for constraint on stitches produced
                              (when (and
                                     (not (zero? out-total))
                                     (not (zero? out-var)))
                                ;; constraint exists
                                ;; find rowinfo index k that corresponds to row number i
                                (let ([k (rowmap-find rowmap i)])
                                  (if (not k)
                                      (error (format "could not find row ~a in pattern" i))
                                      (let ([rowinfo-k (vector-ref rowinfos k)]
                                            [qo (quotient (- out-total out-fixed) out-var)])
                                        (if (negative? qo)
                                            (error (format "pattern row ~a has negative value of variable repeat" i))
                                            (let ([stitches~ (tree-var-replace (Rowinfo-stitches rowinfo-k) qo)]
                                                  [in-total~ (+ in-fixed (* qo in-var))])
                                              (vector-set! rowinfos k
                                                           (Rowinfo
                                                            (if (zero? qo)
                                                                (combine stitches~) ; eliminate variable repeat evaluated at zero
                                                                stitches~)
                                                            (Rowinfo-memo rowinfo-k)
                                                            (Rowinfo-yarn rowinfo-k)
                                                            in-total~
                                                            out-total
                                                            0 0 0 0)))))))))))
                      (j-loop i))))
                ;; recurse
                (pattern-guard-stitch-counts-recurse
                 var-count-total-final name rowinfos rowmap technology geometry startface startside gauge yarns)))
            ;; exit as we have finished constraining the variable repeats
            (values name rowinfos rowmap technology geometry startface startside gauge yarns)))))
  
  ;; composable function as part of Pattern struct guard function
  (: pattern-guard-unconstrained-var : (String
                                        (Vectorof Rowinfo)
                                        Rowmap
                                        (U 'hand 'machine)
                                        (U 'flat 'circular)
                                        (U 'rs 'ws)
                                        (U 'left 'right)
                                        (Option Gauge)
                                        (Immutable-Vectorof (Option yarntype)) ->
                                        (values String
                                                (Vectorof Rowinfo)
                                                Rowmap
                                                (U 'hand 'machine)
                                                (U 'flat 'circular)
                                                (U 'rs 'ws)
                                                (U 'left 'right)
                                                (Option Gauge)
                                                (Immutable-Vectorof (Option yarntype)))))
  (define (pattern-guard-unconstrained-var name rowinfos rowmap technology geometry startface startside gauge yarns)
    ;; check for unconstrained variable repeats
    (let ([var-rownums : (Listof Positive-Index)
                       (let loop ([i : Integer (sub1 (vector-length (Rowmap-data rowmap)))]
                                  [acc : (Listof Positive-Index) null])
                         (if (negative? i)
                             acc
                             (if (zero? (tree-count-var (Rowinfo-stitches (vector-ref rowinfos i))))
                                 (loop (sub1 i) acc)
                                 (loop (sub1 i) (append acc (vector->list (vector-ref (Rowmap-data rowmap) i)))))))])
      (if (not (null? var-rownums))
          (error (string-append "unconstrained variable repeat in row"
                                (format-rows (sort var-rownums <))))
          ;; result
          (values name rowinfos rowmap technology geometry startface startside gauge yarns))))
  
  ;; composable function as part of Pattern struct guard function
  (: pattern-guard-rows-conformable : (String
                                       (Vectorof Rowinfo)
                                       Rowmap
                                       (U 'hand 'machine)
                                       (U 'flat 'circular)
                                       (U 'rs 'ws)
                                       (U 'left 'right)
                                       (Option Gauge)
                                       (Immutable-Vectorof (Option yarntype)) ->
                                       (values String
                                               (Vectorof Rowinfo)
                                               Rowmap
                                               (U 'hand 'machine)
                                               (U 'flat 'circular)
                                               (U 'rs 'ws)
                                               (U 'left 'right)
                                               (Option Gauge)
                                               (Immutable-Vectorof (Option yarntype)))))
  (define (pattern-guard-rows-conformable name rowinfos rowmap technology geometry startface startside gauge yarns)
    ;; check that consecutive rows are conformable
    (let ([n-rows : Natural (Rowmap-rowcount rowmap)])
      (when (> 1 n-rows)
        (let ([stitches-in-total ((inst make-vector Natural) n-rows 0)]
              [stitches-out-total ((inst make-vector Natural) n-rows 0)])
          (for ([i (in-range (vector-length (Rowmap-data rowmap)))])
            (let ([rowinfo-i (vector-ref rowinfos i)]
                  [rownums-i : (Immutable-Vectorof Index) (vector-ref (Rowmap-data rowmap) i)])
              (let ([stitches-in-total-row-i (Rowinfo-stitches-in-total rowinfo-i)]
                    [stitches-out-total-row-i (Rowinfo-stitches-out-total rowinfo-i)])
                (for ([j (in-range (vector-length rownums-i))])
                  (vector-set! stitches-in-total (sub1 (vector-ref rownums-i j)) stitches-in-total-row-i)
                  (vector-set! stitches-out-total (sub1 (vector-ref rownums-i j)) stitches-out-total-row-i)))))
          (when (for/or : Boolean ([i (in-range 1 n-rows)])
                  (not (= (vector-ref stitches-in-total i) (vector-ref stitches-out-total (sub1 i)))))
            (error "pattern rows do not have conformable stitch counts"))))); FIXME identify bad rows in error message
    ;; result
    (values name rowinfos rowmap technology geometry startface startside gauge yarns))

  ;; composable function as part of Pattern struct guard function
  (: pattern-guard-combine-stitches : (String
                                       (Vectorof Rowinfo)
                                       Rowmap
                                       (U 'hand 'machine)
                                       (U 'flat 'circular)
                                       (U 'rs 'ws)
                                       (U 'left 'right)
                                       (Option Gauge)
                                       (Immutable-Vectorof (Option yarntype)) ->
                                       (values String
                                               (Vectorof Rowinfo)
                                               Rowmap
                                               (U 'hand 'machine)
                                               (U 'flat 'circular)
                                               (U 'rs 'ws)
                                               (U 'left 'right)
                                               (Option Gauge)
                                               (Immutable-Vectorof (Option yarntype)))))
  (define (pattern-guard-combine-stitches name rowinfos rowmap technology geometry startface startside gauge yarns)
    ;; combine repeated stitches / nested singletons
    ;; zero out fixed/var totals
    (values
     name
     (for/vector : (Vectorof Rowinfo)
       ([rowinfo-i (vector->list rowinfos)])
       (Rowinfo
        (combine (Rowinfo-stitches rowinfo-i))
        (Rowinfo-memo rowinfo-i)
        (Rowinfo-yarn rowinfo-i)
        (Rowinfo-stitches-in-total rowinfo-i)
        (Rowinfo-stitches-out-total rowinfo-i)
        0 0 0 0))
     rowmap technology geometry startface startside gauge yarns))

  ;; alternative constructor
  (: pattern : (->* () (#:name String
                        #:technology (U 'hand 'machine)
                        #:geometry (U 'flat 'circular)
                        #:startface (U 'rs 'ws)
                        #:startside (U 'left 'right)
                        #:gauge (Option Gauge)
                        #:yarns (Immutable-Vectorof (Option yarntype)))
                    #:rest Rows
                    Pattern))
  (define (pattern
           #:name [name : String ""]
           #:technology [technology : (U 'hand 'machine) 'machine]
           #:geometry [geometry : (U 'flat 'circular) 'flat]
           #:startface [startface : (U 'rs 'ws) 'rs]
           #:startside [startside : (U 'left 'right) 'right]
           #:gauge [gauge : (Option Gauge) #f]
           #:yarns [yarns : (Immutable-Vectorof (Option yarntype)) (vector-immutable #f)]
           . rows)
    (log-knitscheme-debug "in `pattern` constructor")
    (let ([rowinfo~
           (list->vector
            (map (λ ([x : Rows])
                   (Rowinfo
                    (Rows-stitches x)
                    (Rows-memo x)
                    (Rows-yarn x)
                    (Rows-stitches-in-total x)
                    (Rows-stitches-out-total x)
                    (Rows-stitches-in-fixed x)
                    (Rows-stitches-out-fixed x)
                    (Rows-stitches-in-var x)
                    (Rows-stitches-out-var x)))
                 rows))]
          [rowmap~
           (rowmap
            (vector->immutable-vector
             ((inst list->vector (Immutable-Vectorof Positive-Index))
              (map (λ ([x : Rows])
                     (vector->immutable-vector
                      ((inst list->vector Positive-Index)
                       (Rows-rownums x))))
                   rows)))
            1)])
      (log-knitscheme-debug (~a rows))
      (log-knitscheme-debug (~a rowinfo~))
      (log-knitscheme-debug (~a rowmap~))
      ;; result
      (Pattern name rowinfo~ rowmap~ technology geometry startface startside gauge yarns)))

  ;; end of module
  (log-knitscheme-debug "finishing knitscheme-base module definition"))

(require 'knitscheme-base-submodule)

;; set up thread to print output from log receiver
(void
 (thread
  (λ () (let sync-loop ()
               (define v (sync knitscheme-receiver))
               (printf "[~a] ~a\n" (vector-ref v 0) (vector-ref v 1))
               (sync-loop)))))

; macro definitions
(log-knitscheme-debug "start of macro definitions")

(require (for-syntax racket/syntax)) ; needed for `format-id`

(define-syntax-rule (define-variable-repeat-stitch id st)
  (define-syntax (id stx)
    (syntax-case stx ()
      [(_ n) #'(make-leaf n (stitch (stitchtype-rs-symbol st) #f))]
      [_ #'(make-leaf 0 (stitch (stitchtype-rs-symbol st) #f))])))

(define-syntax-rule (define-repeatable-stitch id st)
  (define-syntax-rule (id n)
    (make-leaf n (stitch (stitchtype-rs-symbol st) #f))))

(define-syntax-rule (define-unrepeatable-stitch id st)
  (define-syntax (id stx)
    (syntax-case stx ()
      [_ #'(make-leaf 1 (stitch (stitchtype-rs-symbol st) #f))])))


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

(define-unrepeatable-stitch dropst  (hash-ref stitch-hash #x2c)) ; `drop` is already defined
(define-unrepeatable-stitch k2tog   (hash-ref stitch-hash #x55))
(define-unrepeatable-stitch p2tog   (hash-ref stitch-hash #x57))
(define-unrepeatable-stitch k3tog   (hash-ref stitch-hash #x73))
(define-unrepeatable-stitch p3tog   (hash-ref stitch-hash #x75))
(define-unrepeatable-stitch ssk     (hash-ref stitch-hash #x56))
(define-unrepeatable-stitch ssp     (hash-ref stitch-hash #x58))
(define-unrepeatable-stitch sssk    (hash-ref stitch-hash #x74))
(define-unrepeatable-stitch sssp    (hash-ref stitch-hash #x76))
(define-unrepeatable-stitch yo      (hash-ref stitch-hash #x6f))
(define-unrepeatable-stitch dyo     (hash-ref stitch-hash #x41))
(define-unrepeatable-stitch cdd     (hash-ref stitch-hash #x6a))
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

;; define k1 ... k20, p1 ... p20, etc.
(define-syntax (define-variable-repeat-stitches stx)
  (syntax-case stx ()
    [(_) (let ([make-name (λ (id n) (format-id stx "~a~a" id n))])
           (with-syntax ([((id n name) ...)
                          (map (λ (xs) (list (car xs) (cdr xs) (make-name (car xs) (cdr xs))))
                               (for*/list ([id '(k p ktbl ptbl kb pb bo)] [n (in-range 1 21)])
                                 (cons id n)))])
             #'(begin
                 (define name (id n))
                 ...)))]))
(define-variable-repeat-stitches)

;; tests

(require typed/rackunit)

(when _TEST_

  (log-knitscheme-info "start tests")

  ;; tests of tree functions
  (log-knitscheme-debug "start tests of tree functions")

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

  (check-equal?
   (flatten-tree
    '((2 . #s(stitch 112 0))
      (2
       (1 . #s(stitch 111 0))
       (3 (2 . #s(stitch 84 0))
          (1 . #s(stitch 112 0)))
       (2 . #s(stitch 111 0)))
      (3 . #s(stitch 106 0))))
   '((2 . #s(stitch 112 0))
     (1 . #s(stitch 111 0))
     (2 . #s(stitch 84 0))
     (1 . #s(stitch 112 0))
     (2 . #s(stitch 84 0))
     (1 . #s(stitch 112 0))
     (2 . #s(stitch 84 0))
     (1 . #s(stitch 112 0))
     (3 . #s(stitch 111 0))
     (2 . #s(stitch 84 0))
     (1 . #s(stitch 112 0))
     (2 . #s(stitch 84 0))
     (1 . #s(stitch 112 0))
     (2 . #s(stitch 84 0))
     (1 . #s(stitch 112 0))
     (2 . #s(stitch 111 0))
     (3 . #s(stitch 106 0))))


  ;; tests of `rows` constructor
  (log-knitscheme-debug "start tests of `rows` constructor")

  ;; no variable repeats, combine adjacent leaves
  (check-equal?
   rows(1 #:memo "test of `rows` constructor")(k1 k1)
   (Rows
    '(1)
    '((2 . #s(stitch 107 0))) "test of `rows` constructor" 0 2 2 2 2 0 0))

  ;; no variable repeats, combine leaf in singleton node
  (check-equal?
   rows(1 #:memo "test of `rows` constructor")(x2(k1))
   (Rows
    '(1)
    '((2 . #s(stitch 107 0))) "test of `rows` constructor" 0 2 2 2 2 0 0))

  ;; no variable repeats, multiple simplifications
  (check-equal?
   rows(1 #:memo "test of `rows` constructor")(x2(x3(x4(k1 k1))))
   (Rows
    '(1)
    '((48 . #s(stitch 107 0))) "test of `rows` constructor" 0 48 48 48 48 0 0))

  ;; nonconsecutive rows
  (check-equal?
   rows(1 3 #:memo "test of `rows` constructor")(p2)
   (Rows
    '(1 3)
    '((2 . #s(stitch 112 0))) "test of `rows` constructor" 0 2 2 2 2 0 0))

  ;; variable number repeat evaluates to zero
  (check-equal?
   rows(1 #:stitches 2 #:memo "test of `rows` constructor")(p2 k)
   (Rows
    '(1)
    '((2 . #s(stitch 112 0))) "test of `rows` constructor" 0 2 2 2 2 0 0))

  ;; variable number repeat evaluates negative
  (check-exn
   exn:fail?
   (λ ()
     rows(1 #:stitches 2)(p3 k)))

  ;; consecutive and conformable
  (check-equal?
   rows(1 2 #:memo "test of `rows` constructor")(k2 p2)
   (Rows
    '(1 2)
    '((2 . #s(stitch 107 0)) (2 . #s(stitch 112 0))) "test of `rows` constructor" 0 4 4 4 4 0 0))

  ;; consecutive and conformable, memo, repeated and list-format row numbers
  (check-equal?
   rows('(1 2) '(1) #:memo "test of `rows` constructor")(p2 repeat(yo x3(bo2) twice(yo)) x3(cdd))
   (Rows
    '(1 2)
    '((2 . #s(stitch 112 0))
      (0
       (1 . #s(stitch 111 0))
       (3 (2 . #s(stitch 84 0)))
       (2 (1 . #s(stitch 111 0))))
      (3 (1 . #s(stitch 106 0)))) "test of `rows` constructor" 0 0 0 11 5 6 3))

  ;; consecutive but not conformable
  (check-exn
   exn:fail?
   (λ ()
     rows(1 2)(k1 m)))

  ;; consecutive but not conformable
  (check-exn
   exn:fail?
   (λ ()
     rows(1 2)(k1 ssk)))

  ;; no row numbers
  (check-exn
   exn:fail?
   (λ ()
     rows(null)(k1)))


  ;; tests of yarn functions
  (log-knitscheme-debug "start of tests of yarn functions")

  ;; MC is default yarn
  (check-equal?
   rows(1 #:memo "test of yarn function")(cc1(k1))
   (Rows
    '(1)
    '((1 . #s(stitch 107 1))) "test of yarn function" 0 1 1 1 1 0 0))

  ;; yarn 0 is MC
  (check-equal?
   rows(1 #:yarn 0 #:memo "test of yarn function")(cc2(k1))
   (Rows
    '(1)
    '((1 . #s(stitch 107 2))) "test of yarn function" 0 1 1 1 1 0 0))

  ;; innermost yarn specification has priority
  (check-equal?
   rows(1 #:memo "test of yarn function")(mc(cc3(k1)))
   (Rows
    '(1)
    '((1 . #s(stitch 107 3))) "test of yarn function" 0 1 1 1 1 0 0))

  ;; row #:yarn specification cedes priority
  (check-equal?
   rows(1 #:yarn 1 #:memo "test of yarn function")(k1 cc4(p1))
   (Rows
    '(1)
    '((1 . #s(stitch 107 1))
      (1 . #s(stitch 112 4))) "test of yarn function" 1 2 2 2 2 0 0))

  ;; row #:yarn specification cedes priority
  (check-equal?
   rows(1 #:yarn 5 #:memo "test of yarn function")(mc(k1) p1)
   (Rows
    '(1)
    '((1 . #s(stitch 107 0))
      (1 . #s(stitch 112 5))) "test of yarn function" 5 2 2 2 2 0 0))

  ;; row #:yarn #f specification destroys inner yarn specifications
  (check-equal?
   rows(1 #:yarn #f #:memo "test of yarn function")(cc6(k1) cc7(p1))
   (Rows
    '(1)
    '((1 . #s(stitch 107 #f))
      (1 . #s(stitch 112 #f))) "test of yarn function" #f 2 2 2 2 0 0))

  ;; FIXME need more tests of yarn functions

  ;; tests of `pattern` constructor
  (log-knitscheme-debug "start tests of `pattern` constructor")

  ;; keywords, single row
  (check-equal?
   (pattern #:technology 'hand #:startface 'ws #:startside 'left
            rows(1 #:memo "test of `pattern` constructor")(k1))
   (Pattern
    ""
    (vector (Rowinfo '((1 . #s(stitch 107 0))) "test of `pattern` constructor" 0 1 1 0 0 0 0))
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
   (λ ()
     (pattern #:technology 'machine #:geometry 'circular
              rows(1 #:memo "test of `pattern` constructor")(k1))))

  ;; variable repeat leaf, number of stitches supplied
  (check-equal?
   (pattern rows(1 3 #:stitches 2 #:memo "test of `pattern` constructor")(k m)
            rows(2 4 #:memo "test of `pattern` constructor")(k2tog))
   (Pattern
    ""
    (vector
     (Rowinfo
      '((1 . #s(stitch 107 0))
        (1 . #s(stitch 62 0))) "test of `pattern` constructor" 0 1 2 0 0 0 0)
     (Rowinfo
      '((1 . #s(stitch 85 0))) "test of `pattern` constructor" 0 2 1 0 0 0 0))
    (Rowmap '#(#(1 3) #(2 4)) 4)
    'machine
    'flat
    'rs
    'right
    #f
    '#(#f)))

  ;; variable repeat leaf nested in node, number of stitches supplied
  (check-equal?
   (pattern rows(1 #:stitches 9 #:memo "test of `pattern` constructor")(x3(k1 p)))
   (Pattern
    ""
    (vector
     (Rowinfo
      '((3 (1 . #s(stitch 107 0))
           (2 . #s(stitch 112 0)))) "test of `pattern` constructor" 0 9 9 0 0 0 0))
    (Rowmap '#(#(1)) 1)
    'machine
    'flat
    'rs
    'right
    #f
    '#(#f)))

  ;; variable repeat node, number of stitches supplied, variable repeat replaced in Rows struct guard function
  (check-equal?
   (pattern #:technology 'hand #:geometry 'circular
            rows(seq(1 4) #:stitches 8 #:memo "test of `pattern` constructor")(k1 repeat(k1 p1) k1))
   (Pattern
    ""
    (vector
     (Rowinfo
      '((1 . #s(stitch 107 0))
        (3 (1 . #s(stitch 107 0)) (1 . #s(stitch 112 0)))
        (1 . #s(stitch 107 0))) "test of `pattern` constructor" 0 8 8 0 0 0 0))
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
            rows(1 3 #:stitches 8
                   #:memo "test of `pattern` constructor")(k1 repeat(k1 p1) k1)
                                                          rows(2 4 #:memo "test of `pattern` constructor")(k1 repeat(p1 k1) k1))
   (Pattern
    ""
    (vector
     (Rowinfo
      '((1 . #s(stitch 107 0))
        (3 (1 . #s(stitch 107 0)) (1 . #s(stitch 112 0)))
        (1 . #s(stitch 107 0))) "test of `pattern` constructor" 0 8 8 0 0 0 0)
     (Rowinfo
      '((1 . #s(stitch 107 0))
        (3 (1 . #s(stitch 112 0)) (1 . #s(stitch 107 0)))
        (1 . #s(stitch 107 0))) "test of `pattern` constructor" 0 8 8 0 0 0 0))
    (Rowmap '#(#(1 3) #(2 4)) 4)
    'hand
    'flat
    'rs
    'right
    #f
    '#(#f)))

  ;; repeated application of constraints and simplifications
  (check-equal?
   (pattern #:technology 'hand #:geometry 'flat
            rows(1 5 #:stitches 8 #:memo "test of `pattern` constructor")(k1 repeat(k1 p1) k1)
            rows(2 6 #:memo "test of `pattern` constructor")(k1 repeat(p1 k1) k1)
            rows(3 7 #:memo "test of `pattern` constructor")(k1 x1(repeat(k1 k1)) x1(k1))
            rows(4 8 #:memo "test of `pattern` constructor")(k1 repeat(p2) k1))
   (Pattern
    ""
    (vector
     (Rowinfo
      '((1 . #s(stitch 107 0))
        (3 (1 . #s(stitch 107 0)) (1 . #s(stitch 112 0)))
        (1 . #s(stitch 107 0))) "test of `pattern` constructor" 0 8 8 0 0 0 0)
     (Rowinfo
      '((1 . #s(stitch 107 0))
        (3 (1 . #s(stitch 112 0)) (1 . #s(stitch 107 0)))
        (1 . #s(stitch 107 0))) "test of `pattern` constructor" 0 8 8 0 0 0 0)
     (Rowinfo
      '((8 . #s(stitch 107 0))) "test of `pattern` constructor" 0 8 8 0 0 0 0)
     (Rowinfo
      '((1 . #s(stitch 107 0))
        (6 . #s(stitch 112 0))
        (1 . #s(stitch 107 0))) "test of `pattern` constructor" 0 8 8 0 0 0 0))
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
            rows(1 5 #:memo "test of `pattern` constructor")(k1 repeat(k1 p1) k1)
            rows(2 6 #:memo "test of `pattern` constructor")(k1 repeat(p1 k1) k1)
            rows(3 7 #:memo "test of `pattern` constructor")(k1 repeat(k2) k1)
            rows(4 8 #:memo "test of `pattern` constructor")(k1 repeat(p2) k1)
            row(   9 #:memo "test of `pattern` constructor")(bo8))
   (Pattern
    ""
    (vector
     (Rowinfo
      '((1 . #s(stitch 107 0))
        (3 (1 . #s(stitch 107 0)) (1 . #s(stitch 112 0)))
        (1 . #s(stitch 107 0))) "test of `pattern` constructor" 0 8 8 0 0 0 0)
     (Rowinfo
      '((1 . #s(stitch 107 0))
        (3 (1 . #s(stitch 112 0)) (1 . #s(stitch 107 0)))
        (1 . #s(stitch 107 0))) "test of `pattern` constructor" 0 8 8 0 0 0 0)
     (Rowinfo
      '((8 . #s(stitch 107 0))) "test of `pattern` constructor" 0 8 8 0 0 0 0)
     (Rowinfo
      '((1 . #s(stitch 107 0))
        (6 . #s(stitch 112 0))
        (1 . #s(stitch 107 0))) "test of `pattern` constructor" 0 8 8 0 0 0 0)
     (Rowinfo '((8 . #s(stitch 84 0))) "test of `pattern` constructor" 0 8 0 0 0 0 0))
    (Rowmap '#(#(1 5) #(2 6) #(3 7) #(4 8) #(9)) 9)
    'hand
    'flat
    'rs
    'right
    #f
    '#(#f)))

  ;; constrained by stitches out
  (check-equal?
   (pattern rows(1 #:memo "test of `pattern` constructor")(k m)
            rows(2 #:memo "test of `pattern` constructor")(k2tog))
   (Pattern
    ""
    (vector
     (Rowinfo
      '((1 . #s(stitch 107 0))
        (1 . #s(stitch 62 0))) "test of `pattern` constructor" 0 1 2 0 0 0 0)
     (Rowinfo
      '((1 . #s(stitch 85 0))) "test of `pattern` constructor" 0 2 1 0 0 0 0))
    (Rowmap '#(#(1) #(2)) 2)
    'machine
    'flat
    'rs
    'right
    #f
    '#(#f)))

  ;; constrained by stitches in
  (check-equal?
   (pattern rows(1 #:memo "test of `pattern` constructor")(k2tog)
            rows(2 #:memo "test of `pattern` constructor")(k m))
   (Pattern
    ""
    (vector
     (Rowinfo
      '((1 . #s(stitch 85 0))) "test of `pattern` constructor" 0 2 1 0 0 0 0)
     (Rowinfo
      '((1 . #s(stitch 107 0))
        (1 . #s(stitch 62 0))) "test of `pattern` constructor" 0 1 2 0 0 0 0))
    (Rowmap '#(#(1) #(2)) 2)
    'machine
    'flat
    'rs
    'right
    #f
    '#(#f)))

  ;; constrained by both stitches in and stitches out
  (check-equal?
   (pattern rows(1 3 5 #:memo "test of `pattern` constructor")(k2tog)
            rows(2 4 #:memo "test of `pattern` constructor")(k(0) m))
   (Pattern
    ""
    (vector
     (Rowinfo
      '((1 . #s(stitch 85 0))) "test of `pattern` constructor" 0 2 1 0 0 0 0)
     (Rowinfo
      '((1 . #s(stitch 107 0))
        (1 . #s(stitch 62 0))) "test of `pattern` constructor" 0 1 2 0 0 0 0))
    (Rowmap '#(#(1 3 5) #(2 4)) 5)
    'machine
    'flat
    'rs
    'right
    #f
    '#(#f)))

  ;; variable repeat evaluates to zero
  (check-equal?
   (pattern rows(1 #:memo "test of `pattern` constructor")(p1 k)
            rows(2 #:memo "test of `pattern` constructor")(p1))
   (Pattern
    ""
    (vector
     (Rowinfo '((1 . #s(stitch 112 0))) "test of `pattern` constructor" 0 1 1 0 0 0 0)
     (Rowinfo '((1 . #s(stitch 112 0))) "test of `pattern` constructor" 0 1 1 0 0 0 0))
    (Rowmap '#(#(1) #(2)) 2)
    'machine
    'flat
    'rs
    'right
    #f
    '#(#f)))

  ;; unconstrained variable repeat (single row)
  (check-exn
   exn:fail?
   (λ ()
     (pattern row(1)(k))))

  ;; unconstrained variable repeat (two rows)
  (check-exn
   exn:fail?
   (λ ()
     (pattern row(1)(k)
              row(2)(p))))

  ;; unconstrained variable repeat (multiple rows)
  (check-exn
   exn:fail?
   (λ ()
     (pattern row(1)(k)
              row(2)(k p)
              row(3)(p)
              row(4)(p k1))))

  ;; wrong number of stitches supplied
  (check-exn
   exn:fail?
   (λ ()
     (pattern rows(1 3 #:stitches 3)(k1 m) rows(2 4)(k2tog))))

  ;; row numbers do not start at 1
  (check-exn
   exn:fail?
   (λ ()
     (pattern rows(2 4)(k1 m) rows(3 5)(k2tog))))

  ;; non-conformable consecutive rows (caught in Row struct guard function)
  (check-exn
   exn:fail?
   (λ ()
     (pattern rows(1 2)(k1 m) rows(3)(k2tog))))

  ;; non-conformable consecutive rows (caught in Pattern struct guard function}
  (check-exn
   exn:fail?
   (λ ()
     (pattern rows(1 3)(k1 m) rows(2 4 5)(k2tog))))

  ;; non-consecutive row numbers (caught in Rowmap struct guard function}
  (check-exn
   exn:fail?
   (λ ()
     (pattern rows(1 3)(k1 m) rows(2 5)(k2tog))))

  ;; too many yarns
  (check-exn
   exn:fail?
   (λ ()
     (pattern #:yarns (vector->immutable-vector (build-vector 257 (λ (x) #s(yarntype "" "" "" ""))))
              rows(1)(k1))))

  (log-knitscheme-info "tests completed"))

(log-knitscheme-info "end of knitscheme-chart.rkt")
;; end