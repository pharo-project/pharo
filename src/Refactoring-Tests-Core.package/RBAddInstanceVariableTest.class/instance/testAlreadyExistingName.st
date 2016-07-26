failure tests
testAlreadyExistingName
	self
		shouldFail: (RBAddInstanceVariableRefactoring 
				variable: 'class'
				class: RBTransformationRuleTest);
		shouldFail: (RBAddInstanceVariableRefactoring 
				variable: 'name'
				class: RBTransformationRuleTest)