# Arith

[![Build Status][badge]][status]

Arith is simple untyped language, first exercise from Benjamin Pierce's book ["Types and programming languages"][tapl].

## EBNF

    term
      = 'true'
      | 'false'
      | 'if' spaces term spaces 'then' spaces term spaces 'else' spaces term
      | 'zero'
      | 'succ' spaces term
      | 'pred' spaces term
      | 'iszero' spaces term
      | '(' term ')'
      .

    spaces = space {space} .

	space = '\t' | '\n' | '\r'| '\f' | '\v' | Unicode space character .

[status]: https://travis-ci.org/sirikid/arith
[badge]: https://travis-ci.org/sirikid/arith.svg?branch=master
[tapl]: https://www.cis.upenn.edu/~bcpierce/tapl/
