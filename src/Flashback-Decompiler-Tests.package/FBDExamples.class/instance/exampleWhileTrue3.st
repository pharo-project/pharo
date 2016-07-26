examples - loops
exampleWhileTrue3
	| t1 t2 |
	t2 := 5.
	t1 := 1.
	t1 := t1+1.
	[ t1 <= t2 ]
		whileTrue: [ 
			t1 + 2.
			t1 := t1 + 1 ]