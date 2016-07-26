examples - loops
exampleWhileTrue
	| array t |
	array := {true.	true.true}.
	t := 1.
	[ t < 4 ]
		whileTrue: [ 
			array at: t put: false.
			t := t + 1 ]