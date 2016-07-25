failure tests
testNonExistantName
	self shouldFail: (RBAbstractInstanceVariableRefactoring 
			variable: 'foo'
			class: RBBasicLintRuleTest)