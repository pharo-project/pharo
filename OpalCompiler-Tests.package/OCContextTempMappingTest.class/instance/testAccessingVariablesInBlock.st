tests
testAccessingVariablesInBlock
	| theContext |
	[ | t1 |
		t1 := true.
		theContext := thisContext.
		self assert: (thisContext tempNamed: 'theContext') == thisContext.
		self assert: (theContext tempNamed: 't1').
	] value
	