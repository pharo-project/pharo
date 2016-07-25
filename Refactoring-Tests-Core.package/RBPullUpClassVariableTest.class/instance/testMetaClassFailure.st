failure tests
testMetaClassFailure
	self shouldFail: (RBPullUpClassVariableRefactoring 
			variable: #RecursiveSelfRule
			class: RBLintRuleTest class)