failure tests
testMetaClassFailure
	self shouldFail: (RBAddClassVariableRefactoring 
			variable: #VariableName
			class: RBTransformationRuleTest class)