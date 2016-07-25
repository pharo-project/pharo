failure tests
testAlreadyExistingName
	self shouldFail: (RBRenameClassVariableRefactoring 
			rename: #RecursiveSelfRule
			to: self objectClassVariable
			in: RBTransformationRuleTest)