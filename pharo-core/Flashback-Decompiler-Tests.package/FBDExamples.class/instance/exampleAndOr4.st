examples-blocks-optimized
exampleAndOr4
	| tmp1 tmp2 |
	tmp1 := true.
	tmp2 := false.
	^ tmp2
		ifTrue: [ true ]
		ifFalse: [ tmp1 ]