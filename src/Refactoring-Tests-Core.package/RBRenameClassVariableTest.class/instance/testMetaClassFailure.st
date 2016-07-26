failure tests
testMetaClassFailure
	self shouldFail: (RBRenameClassVariableRefactoring 
			rename: #RecursiveSelfRule
			to: #Foo
			in: RBTransformationRuleTest class)