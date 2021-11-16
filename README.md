# knitscheme

Domain Specific Language for knitting patterns

## Description

DSL for knitting patterns with surface syntax
resembling Knitspeak, intended as a target for
editors and parsers.

## Motivation

Knitspeak is expressive and relatively compact.
Above all, it is readable by humans. On the downside,
it lacks a specification. The [Stitch Maps](https://stitch-maps.com)
website provides a *de facto* standard, but the
parser makes arbitrary decisions to resolve corner
cases. For example, the following specifications
result in patterns of different width:

    Row 1: knit (99 sts).
    Row 2: purl to last st, knit 1.

[link](https://stitch-maps.com/patterns/display/25338/)

    Row 1: knit 99 sts.
    Row 2: purl to last st, knit 1.

[link](https://stitch-maps.com/patterns/display/25349/)

(Note that the written instructions on the webpage 
have changed because they are back-generated after
parsing.)

## Implementation

The current implementation has been coded as a module for
[Typed Racket](https://docs.racket-lang.org/ts-guide/).

Patterns are encoded in a format that is easy for humans
to write and parse, but is also highly structured. Output
formats include written instructions for hand knitting,
and charts that graphically represent the stitches and
the color of the yarn they are knitted in.


