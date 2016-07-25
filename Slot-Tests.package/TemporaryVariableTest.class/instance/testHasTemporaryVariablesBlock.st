tests
testHasTemporaryVariablesBlock
	| block |
	block := [ | b | b := 2. b +2  ].
	self assert: (block hasTemporaryVariableNamed: #b).