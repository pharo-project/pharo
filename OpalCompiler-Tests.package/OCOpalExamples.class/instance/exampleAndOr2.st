examples-blocks-optimized
exampleAndOr2
	| t1 t2 |
	t1 := true.
	t2 := false.  
	t2 or: [ t1. '2' logCr ]. "effect" 