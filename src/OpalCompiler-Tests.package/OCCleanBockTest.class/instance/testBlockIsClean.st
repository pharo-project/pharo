tests
testBlockIsClean
	self assert: [  ] sourceNode isClean.
	self assert: [ 1 + 2 ] sourceNode isClean.
	self assert: [ :a | a + 2 ] sourceNode isClean.
	self assert: [ :a :b | a + b + 3 ] sourceNode isClean.
	self assert: [ | a | a := 1. a + 3 ] sourceNode isClean.
	 