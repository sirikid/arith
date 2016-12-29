# Arith

[![Build Status](https://travis-ci.org/sirikid/arith.svg?branch=master)][build_status]

Arith is simple untyped language, first exercise from Benjamin Pierce's book ["Types and programming languages"][tapl].

## BNF

	term = 'zero'
		| 'true'
		| 'false'
		| 'succ' term
		| 'pred' term
		| 'is_zero' term
		| 'if' term 'then' term 'else' term


[build_status]: https://travis-ci.org/sirikid/arith
[tapl]: https://www.cis.upenn.edu/~bcpierce/tapl/

