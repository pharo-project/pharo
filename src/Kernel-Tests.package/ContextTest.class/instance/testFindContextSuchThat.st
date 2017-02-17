tests
testFindContextSuchThat
	self assert: (aMethodContext findContextSuchThat: [:each| true]) printString = aMethodContext printString.
	self assert: (aMethodContext hasContext: aMethodContext). 