tests
testPush
	
	| aStack |
	aStack := Stack new.
	aStack push: 'a'.
	self assert: aStack size = 1.	
	aStack push: 'b'.
	self assert: aStack size = 2.
	