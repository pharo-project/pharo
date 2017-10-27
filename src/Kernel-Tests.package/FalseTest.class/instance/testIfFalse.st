tests - controlling
testIfFalse
	
	self assert: ((false ifFalse: ['alternativeBlock']) = 'alternativeBlock'). 