examples - loops
exampleWhileTrue4
	| tmp1 tmp2 |
	tmp1 := 1.
	[ tmp1 <= 5 ]
		whileTrue: [ 
			tmp1 + 2.
			tmp1 := tmp1 + 1 ].
	tmp2 := 1.
	[ tmp2 <= 5 ]
		whileTrue: [ 
			tmp1 := tmp2.
			tmp2 := tmp2 + 1 ]