#lang knit typed/racket
require("knitscheme.rkt")

define(demo
       pattern([work hand]
               [form flat]
               [face rs]
               [side right]
               [yarns list(yarn("pink") yarn("sky blue"))]
               rows(1 9)(k8)
               rows(2)(k2 k2tog ssk k2)
               rows(3 [yarn cc1])(k1 k2tog ssk k1)
               rows(4)(k2tog ssk)
               rows(5)(k2)
               rows(6)(k1 cc1(ml mr k1))
               rows(7)(k2 ml mr k2)
               rows(8 [yarn cc1])(k3 ml mc(mr k3))))
display(pattern->text(demo))
pattern->chart(demo)