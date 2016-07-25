failure tests
testNonExistantName
	self shouldFail: (RBPullUpClassVariableRefactoring 
			variable: #Foo
			class: RBLintRuleTest)