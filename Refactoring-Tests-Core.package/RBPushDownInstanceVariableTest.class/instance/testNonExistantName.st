failure tests
testNonExistantName
	self shouldFail: (RBPushDownInstanceVariableRefactoring 
			variable: 'foo'
			class: RBBasicLintRuleTest)