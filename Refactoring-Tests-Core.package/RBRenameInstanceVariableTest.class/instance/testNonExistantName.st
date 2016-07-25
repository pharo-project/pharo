failure tests
testNonExistantName
	self shouldFail: (RBRenameInstanceVariableRefactoring 
			rename: 'foo'
			to: 'newFoo'
			in: RBBasicLintRuleTest)