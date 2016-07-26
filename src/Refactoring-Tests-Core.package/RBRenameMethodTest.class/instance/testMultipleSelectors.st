failure tests
testMultipleSelectors
	self shouldWarn: (RBRenameMethodRefactoring 
			renameMethod: #checkClass:
			in: RBBasicLintRuleTest
			to: #foo:
			permutation: (1 to: 1))