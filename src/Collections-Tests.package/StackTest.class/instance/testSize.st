tests
testSize
	
	| aStack |
	aStack := Stack new.
	self assert: aStack size = 0.
	aStack push: 'a'.
	self assert: aStack size = 1.
	aStack push: 'b'.
	self assert: aStack size = 2.
	aStack pop.
	self assert: aStack size = 1.
	aStack pop.
	self assert: aStack size = 0.

 
	


