examples - loops
exampleWhileFalse
	| t |
	t := 1.
	[ t > 10 ] whileFalse: [ t := t + 1. 1 + 2 ]