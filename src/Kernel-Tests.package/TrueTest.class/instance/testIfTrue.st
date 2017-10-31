tests - controlling
testIfTrue

	self assert: ((true ifTrue: ['alternativeBlock']) = 'alternativeBlock'). 