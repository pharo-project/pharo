failure tests
testReferencedVariable
	self shouldFail: (RBRemoveClassVariableRefactoring 
			variable: #RecursiveSelfRule
			class: RBTransformationRuleTest)