failure tests
testBadName
	self
		shouldFail: (RBRenameClassRefactoring 
				rename: RBLintRuleTest
				to: self objectClassVariable);
		shouldFail: (RBRenameClassRefactoring 
				rename: RBLintRuleTest
				to: #'Ob ject')