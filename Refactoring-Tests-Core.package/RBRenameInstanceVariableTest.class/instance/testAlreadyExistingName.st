failure tests
testAlreadyExistingName
	self shouldFail: (RBRenameInstanceVariableRefactoring 
			rename: 'classBlock'
			to: 'name'
			in: RBBasicLintRuleTest)