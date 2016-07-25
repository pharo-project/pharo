failure tests
testExistingSelector
	self shouldFail: (RBRenameMethodRefactoring 
			renameMethod: #checkClass:
			in: RBBasicLintRuleTest
			to: #runOnEnvironment:
			permutation: (1 to: 1))