examples-blocks-optimized
exampleAndOr2
	| tmp1 tmp2 |
	tmp1 := true.
	tmp2 := false.
	tmp2
		ifTrue: [  ]
		ifFalse: [ '2' logCr ].
	^ self