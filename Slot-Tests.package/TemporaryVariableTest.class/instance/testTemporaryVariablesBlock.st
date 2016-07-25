tests
testTemporaryVariablesBlock
	| block |
	block := [ | b | b +2  ].
	self assert: block temporaryVariables first name = #b.