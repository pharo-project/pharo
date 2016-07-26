examples-blocks-optimized
exampleWhileNoModification
	"tempvars aren't modified in the loop, therefore they don't need to be in a indirection vector.
	  number of temps = declared temps "

	| a b c |
	a := 1.
	b := 2.
	c := 3.
	[ false ]
		whileTrue: [ 
			[ a class.
			  b class.
			  c class ]	"<- block in optimized loop" ]