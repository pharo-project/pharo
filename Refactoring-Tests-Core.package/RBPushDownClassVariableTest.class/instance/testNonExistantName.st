failure tests
testNonExistantName
	self shouldFail: (RBPushDownClassVariableRefactoring 
			variable: #Foo
			class: RBBasicLintRuleTest)