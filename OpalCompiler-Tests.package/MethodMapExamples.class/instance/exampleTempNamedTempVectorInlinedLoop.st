examples
exampleTempNamedTempVectorInlinedLoop
"This test is for accessing temp var c within an inlined loop scope. 
Temp var c is written in the loop and closed in a block -> therefore it is in a tempvector. "
	| a b c |
	c := nil.
	a := 1.
	[ c := 42.
	b := (DebuggerMethodMapOpal forMethod: thisContext method) tempNamed:'c' in: thisContext. a < 1 ]
		whileTrue: [ a := a + 1.
				[ c asString ] ].
	^ b