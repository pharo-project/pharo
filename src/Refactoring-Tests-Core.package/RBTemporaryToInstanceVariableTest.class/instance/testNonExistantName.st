failure tests
testNonExistantName
	self
		shouldFail: (RBTemporaryToInstanceVariableRefactoring 
				class: RBBasicLintRuleTest
				selector: #checkClass:
				variable: 'asdf');
		shouldFail: (RBTemporaryToInstanceVariableRefactoring 
				class: RBBasicLintRuleTest
				selector: #checkClass1:
				variable: 'aSmalllintContext')