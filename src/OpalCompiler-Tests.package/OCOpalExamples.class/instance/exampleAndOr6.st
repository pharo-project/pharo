examples-blocks-optimized
exampleAndOr6
	| t1 t2 |
	t1 := true.
	t2 := false.  
	^ t1 and: [ t2 or: [ t1 ] ]