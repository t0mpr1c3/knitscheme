#lang knit typed/racket
(provide (all-defined-out))

;; FIXME need documentation

;; FIXME is there a better way than this to set compile-time variables?
(define _KNITSCHEME-TEXT/TEST_ #t)

(require typed/racket
         "knitscheme-base.rkt"
         racket/list) ; needed for `flatten` `range`

;; text output functions

(: yarn->text : ((Option Byte) -> String))
(define (yarn->text y)
  (if (false? y)
      ""
      (if (zero? y)
          "MC"
          (format "CC~a" y))))

(: inyarn->text : ((Option Byte) -> String))
(define (inyarn->text y)
  (if (false? y)
      ""
      (string-append "in " (yarn->text y))))

;; write paragraph describing yarns
;; commented out code inserts field names into yarn description
(: yarns->text : ((Immutable-Vectorof (Option yarntype))-> String))
(define (yarns->text yarns)
  (let ([n (vector-length yarns)])
    (if (zero? n)
        ""
        (string-append
         "\nYarn:\n"
         (let loop : String ([i 0]
                             [res : String ""])
           (if (< i n)
               (loop (add1 i)
                     (string-append
                      res
                      (yarn->text (cast i (Option Byte)))
                      (let ([yarn-i (vector-ref yarns i)])
                        (if (false? yarn-i)
                            ""
                            (let* ([brand-i (yarntype-brand yarn-i)]
                                   [fiber-i (yarntype-fiber yarn-i)]
                                   [weight-i (yarntype-weight yarn-i)]
                                   [color-i (yarntype-color yarn-i)]
                                   #| [attr (list "color" "fiber" "weight" "brand")] |#
                                   [desc (list color-i weight-i fiber-i brand-i)]
                                   [empty (map (λ ([x : String]) (string=? "" x))
                                               desc)]
                                   [str (map (λ ([x : String] #| [y : String] |#)
                                               (string-append x #| " " y |#))
                                             #| attr |#
                                             desc)]
                                   [str-pairs (map (inst cons Boolean String)
                                                   empty
                                                   str)]
                                   [str-pairs-filtered (filter (λ ([x : (Pairof Boolean String)]) (not (car x)))
                                                               str-pairs)]
                                   [str-filtered (map (inst cdr Boolean String)
                                                      str-pairs-filtered)])
                              (string-append
                               (if (andmap (inst identity Boolean)
                                           empty)
                                   ""
                                   " - ")
                               (string-join str-filtered #| ", " |# )))))
                      "\n"))
               res))))))

;; chop last letter of string
(: string-chop-last : (String -> String))
(define (string-chop-last x)
  (substring x 0 (sub1 (string-length x))))

;; format stitch tree for text output
(: stitches->text : (Tree -> String))
(define (stitches->text tree)
  (string-chop-last ; remove trailing comma
   (let x-loop ([z : Tree tree]
                [last-yarn : (Option Byte) #f]
                [y : String ""])
     (if (null? z)
         y
         (let ([x : (U Leaf Node) (car z)])
           (if (Leaf? x)
               ;; leaf
               (let ([s (hash-ref stitch-hash (leaf-stitchtype x))]
                     [n (leaf-count x)]
                     [c (leaf-yarntype x)])
                 (x-loop (cdr z)
                         c
                         (string-append
                          (if (false? last-yarn)
                              (string-append y " " (inyarn->text c))
                              (if (not (eq? last-yarn c))
                                  (string-append (string-chop-last y) "; " (inyarn->text c))
                                  y)) " "
                                      (if (stitchtype-repeatable s)
                                          (string-append (stitchtype-id s) (~a n) "," )
                                          (string-append (stitchtype-id s)
                                                         (if (= n 1)
                                                             ""
                                                             (format " ~a times" n))
                                                         ",")))))
               ;; node
               (let ([t (node-tree x)]
                     [n (node-count x)])
                 (string-append (string-chop-last y) "; [" (stitches->text t) " ] "
                                (if (= n 1)
                                    "once"
                                    (if (= n 2)
                                        "twice"
                                        (format "~a times" n)))
                                ","))))))))

;; format pattern for text output
(: pattern->text : (Pattern -> String))
(define (pattern->text p)
  (let* ([hand? : Boolean (eq? 'hand (Pattern-work p))]
         [flat? : Boolean (eq? 'flat (Pattern-form p))]
         [rs? : Boolean (eq? 'rs (Pattern-face p))]
         [row-lex (if flat?
                      "Row"
                      "Round")]
         [rowmap (Pattern-rowmap p)]
         [rownums (Rowmap-data rowmap)]
         ;; order rowinfos by minimum row number
         [rowinfos-pairs ((inst map (Pairof Natural Natural) Natural Natural)
                          cons
                          (map (λ ([xs : (Vectorof Natural)]) (apply min (vector->list xs)))
                               (vector->list rownums))
                          (range (vector-length rownums)))]
         [rowinfos-ordered (sort rowinfos-pairs
                                 (λ ([x : (Pairof Natural Natural)] [y : (Pairof Natural Natural)])
                                   (< (car x) (car y))))]
         [rowinfos-order ((inst map Natural (Pairof Natural Natural)) cdr rowinfos-ordered)]
         [n : Natural (length rowinfos-order)])
    ;; loop over rowinfos
    (let loop ([i : Natural 0]
               [res : String
                    (string-append

                     ;; initial paragraph describing pattern options
                     "This " (symbol->string (Pattern-work p))
                     " knitting pattern is designed to be knit "
                     (if flat?
                         (string-append "flat. "
                                        (if hand?
                                            "Odd-numbered rows are"
                                            "Every row is"))
                         "in the round. Every round is")
                     " knit on the " (if rs?
                                         "RS"
                                         "WS") " of the piece"
                                               (if (and flat? hand?)
                                                   (string-append ", even-numbered rows on the " (if rs? "WS" "RS"))
                                                   "")
                                               ". "
                                               (if (and flat? hand?)
                                                   "The first "
                                                   "Each ") (string-downcase row-lex)
                                                            " starts on the " (symbol->string (Pattern-side p))
                                                            " hand side of the pattern.\n"

                                                            ;; paragraph describing yarns
                                                            (yarns->text (Pattern-yarns p))

                                                            ;; beginning of knitting instructions
                                                            "\nCast on "
                                                            (~a (Rowinfo-stitches-in-total (vector-ref (Pattern-rowinfos p) 0)))
                                                            " stitches"
                                                            (if flat?
                                                                ""
                                                                " and join in the round")
                                                            ".\n")])
      (if (< i n)
          (let* ([j (list-ref rowinfos-order i)]
                 [rownums-j (vector->list (vector-ref rownums j))]
                 [rowinfo-j (vector-ref (Pattern-rowinfos p) j)]
                 [memo-j (Rowinfo-memo rowinfo-j)]
                 [last-row? (not (false? (member (Rowmap-rowcount rowmap) rownums-j)))])
            (loop (add1 i)
                  (string-append res
                                 ;; row/round lexeme
                                 row-lex
                                 ;; row numbers
                                 (format-rows rownums-j)
                                 ":"
                                 ;; stitches
                                 (stitches->text (Rowinfo-stitches rowinfo-j))
                                 ;; annotations
                                 (let ([annot : (Listof String)
                                              (filter-not
                                               (λ ([x : String]) (zero? (string-length x)))
                                               (list
                                                ;; memo
                                                (if (zero? (string-length memo-j))
                                                    ""
                                                    (string-append #| "memo " |# memo-j))
                                                ;; number of stitches
                                                ;; FIXME should be last line of stitches, not last line of pattern!
                                                (if last-row? ; (= i (sub1 n))
                                                    (string-append (~a (Rowinfo-stitches-out-total rowinfo-j))
                                                                   " stitches")
                                                    "")))])
                                   (if (null? annot)
                                       ""
                                       (string-append " (" (string-join annot "; ") ")")))
                                 ".\n")))
          res))))

;; tests

(require typed/rackunit)

(when _KNITSCHEME-TEXT/TEST_

  (log-knitscheme-info "start knitscheme-text tests")

  ;; tests of `yarns->text` function
  (log-knitscheme-debug "start tests of `yarns->text` function")

  (check-equal?
   (yarns->text '#())
   "")

  (check-equal?
   (yarns->text '#(#s(yarntype "" "" "" "")))
   "\nYarn:\nMC\n")

  (check-equal?
   (yarns->text '#(#f #s(yarntype "red" "worsted" "wool" "unknown")))
   "\nYarn:\nMC\nCC1 - red worsted wool unknown\n")

  (log-knitscheme-info "knitscheme-text tests completed"))

(log-knitscheme-info "end of knitscheme-text.rkt")
;; end
