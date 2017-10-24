tests
testTop

	| aStack |
	aStack := Stack new.
	self assert: aStack isEmpty.
	aStack push: 'a'.
	aStack push: 'b'.
	self assert: aStack top = 'b'.
	self assert: aStack top = 'b'.
	self assert: aStack size = 2.