examples - loops
example2Loops2
	| t |
	t := 1.
	[ t < 5 ] whileTrue: [ [ t > 10 ] whileFalse: [ t := t * 2 ]. t := t + 1. ].