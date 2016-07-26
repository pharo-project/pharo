testing
testNewSlice
	| newSlice |
	
	newSlice:= RGSlice named: 'Foo'.
	
	self assert: (newSlice isSlice).
	self assert: (newSlice classes isEmpty).
	self assert: (newSlice environment == Smalltalk globals)