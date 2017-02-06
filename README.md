# Arith

[![Build Status][build_status_badge]][build_status]

Arith is simple untyped language, first exercise from Benjamin Pierce's book ["Types and programming languages"][tapl].

## BNF

	expression = term [space] ';'

	term = 'zero'
		| 'true'
		| 'false'
		| 'succ' {space} term
		| 'pred' {space} term
		| 'is_zero' {space} term
		| 'if' {space} term {space} 'then' {space} term {space} 'else' {space} term

	space = '\t' | '\n' | '\r'| '\f' | '\v' | Unicode space character


[build_status]: https://travis-ci.org/sirikid/arith
[build_status_badge]: https://travis-ci.org/sirikid/arith.svg?branch=master
[tapl]: https://www.cis.upenn.edu/~bcpierce/tapl/

