failure tests
testBadInitializationCode
	self
		shouldFail: (RBAddParameterRefactoring 
				addParameterToMethod: #name
				in: RBLintRuleTest
				newSelector: #name:
				initializer: 'foo:');
		shouldFail: (RBAddParameterRefactoring 
				addParameterToMethod: #name
				in: RBLintRuleTest
				newSelector: #name:
				initializer: 'foo')