failure tests
testAlreadyExistingName
	self
		shouldFail: (RBAddClassVariableRefactoring 
				variable: #RecursiveSelfRule
				class: RBTransformationRuleTest);
		shouldFail: (RBAddClassVariableRefactoring 
				variable: self objectClassVariable
				class: RBTransformationRuleTest)