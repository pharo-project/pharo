failure tests
testMetaClassFailure
	self shouldFail: (RBAbstractClassVariableRefactoring 
			variable: #RecursiveSelfRule
			class: RBTransformationRuleTest class)