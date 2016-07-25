tests
testReadTemporaryVariablesMethod
	| tempVar |
	tempVar := thisContext temporaryVariableNamed: #tempVar.

	self assert: (tempVar readInContext: thisContext) class = TemporaryVariable.