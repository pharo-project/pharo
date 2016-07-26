failure tests
testPushDownNonExistantMenu
	| refactoring |
	refactoring := RBPushDownMethodRefactoring 
		pushDown: #(#someMethodThatDoesNotExist )
		from: RBLintRuleTest.
	self shouldFail: refactoring