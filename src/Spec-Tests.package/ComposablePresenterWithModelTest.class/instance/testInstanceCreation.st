testing
testInstanceCreation

	| aPoint presenter |
	
	aPoint := 40@54.
	presenter := TestingComposablePresenterWithModel on: aPoint.
	
	self assert: presenter model equals: aPoint.
	self deny: (aPoint dependents includes: presenter).
	
	
	