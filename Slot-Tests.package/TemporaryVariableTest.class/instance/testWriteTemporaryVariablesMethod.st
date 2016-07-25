tests
testWriteTemporaryVariablesMethod
	| tempVar |
	tempVar := thisContext temporaryVariableNamed: #tempVar.

	tempVar write: 5 InContext: thisContext.
	self assert: tempVar = 5.