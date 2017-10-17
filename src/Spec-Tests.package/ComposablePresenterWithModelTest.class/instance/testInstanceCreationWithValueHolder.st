testing
testInstanceCreationWithValueHolder

	| point valueHolder presenter |
	
	point := (40@54).
	valueHolder := point asValueHolder.
	presenter := TestingComposablePresenterWithModel on: valueHolder.
	
	self assert: presenter model equals: point.
	
	
	
	