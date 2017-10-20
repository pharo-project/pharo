tests
testPop

	| aStack res elem |
	elem := 'anElement'.	
	aStack := Stack new.
	self assert: aStack isEmpty.
	
	aStack push: 'a'.
	aStack push: elem.
	res := aStack pop.	
	self assert: res = elem.
	self assert: res == elem.
	
	self assert: aStack size = 1.
	aStack pop.
	self assert: aStack isEmpty.

