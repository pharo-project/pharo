tests
testNonExistantName
	self
		shouldFail: (RBMoveVariableDefinitionRefactoring 
				bindTight: (1 to: 10)
				in: RBLintRuleTest
				selector: #name1);
		shouldFail: (RBMoveVariableDefinitionRefactoring 
				bindTight: (self 
						convertInterval: (44 to: 54)
						for: (RBLintRuleTest sourceCodeAt: #displayName))
				in: RBLintRuleTest
				selector: #displayName);
		shouldFail: (RBMoveVariableDefinitionRefactoring 
				bindTight: (self 
						convertInterval: (16 to: 25)
						for: (RBLintRuleTest sourceCodeAt: #displayName))
				in: RBLintRuleTest
				selector: #displayName)