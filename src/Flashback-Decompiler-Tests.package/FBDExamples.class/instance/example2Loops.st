examples - loops
example2Loops
	| t |
	t := 1.
	[ t < 5 ] whileTrue: [ t := t + 1. ].
	[ t > 10 ] whileFalse: [ t := t * 2 ].