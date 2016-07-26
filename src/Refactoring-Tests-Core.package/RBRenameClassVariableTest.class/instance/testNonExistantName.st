failure tests
testNonExistantName
	self shouldFail: (RBRenameClassVariableRefactoring 
			rename: #foo
			to: #newFoo
			in: RBBasicLintRuleTest)