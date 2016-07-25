failure tests
testNonExistantName
	self
		shouldFail: (RBCreateAccessorsForVariableRefactoring 
				variable: #Foo
				class: RBBasicLintRuleTest
				classVariable: true);
		shouldFail: (RBCreateAccessorsForVariableRefactoring 
				variable: 'foo'
				class: RBBasicLintRuleTest
				classVariable: true)