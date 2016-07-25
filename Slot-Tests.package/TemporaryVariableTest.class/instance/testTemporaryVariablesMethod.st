tests
testTemporaryVariablesMethod
	| method |
	method := self class >> #testTemporaryVariablesMethod.
	self assert: method temporaryVariables first name = #method.