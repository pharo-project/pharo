generators
fibonacciSequence
	"Yields an infinite sequence of fibonacci numbers."
	
	^ Generator on: [ :generator |
		| a b |
		a := 0. b := 1.
		[ a := b + (b := a).
		  generator yield: a ]
			repeat ]