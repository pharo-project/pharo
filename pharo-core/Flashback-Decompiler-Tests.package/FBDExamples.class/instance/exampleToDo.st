examples - loops
exampleToDo
	| tmp1 tmp2 tmp3 |
	tmp1 := {true.
	true.
	true}.
	tmp2 := tmp1 size.
	tmp3 := 1.
	[ tmp3 <= tmp2 ]
		whileTrue: [ 
			tmp1 at: tmp3 put: false.
			tmp3 := tmp3 + 1 ].
	^ 1