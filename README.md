# Arith

[![Build Status][badge]][status]

Arith is simple untyped language, first exercise from Benjamin Pierce's book ["Types and programming languages"][tapl].

## EBNF

    term =
      | true
      | false
      | if term then term else term
      | 0
      | succ term
      | pred term
      | iszero term
      | ( term )

[status]: https://travis-ci.org/sirikid/arith
[badge]: https://travis-ci.org/sirikid/arith.svg?branch=master
[tapl]: https://www.cis.upenn.edu/~bcpierce/tapl/
