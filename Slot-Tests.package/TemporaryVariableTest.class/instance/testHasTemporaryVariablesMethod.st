tests
testHasTemporaryVariablesMethod
	| method |
	method := self class >> #testTemporaryVariablesMethod.
	self assert: (method hasTemporaryVariableNamed: #method).