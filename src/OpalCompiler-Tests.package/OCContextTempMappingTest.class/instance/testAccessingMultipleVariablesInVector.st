tests
testAccessingMultipleVariablesInVector
	| t1 t2 t3 t4 t5 t6 |
	
	"Check the source code availability to do not fail on images without sources"
	thisContext method hasSourceCode ifTrue: [ ^ self ].
	
	t1 := 1.
	t2 := 2.
	t3 := 3.
	t4 := 4. 
	t5 := 5 .
	t6 := 6.
	[t5 := 50.
	    t6 := 60.
	    t3 + t4.
	] value.
	
	self assert: t1 == (thisContext tempNamed: 't1').
	self assert: t2 == (thisContext tempNamed: 't2').
	self assert: t3 == (thisContext tempNamed: 't3').
	self assert: t4 == (thisContext tempNamed: 't4').
	self assert: t5 == (thisContext tempNamed: 't5').
	self assert: t6 == (thisContext tempNamed: 't6').