failure tests
testRemoveClassWithReferencesRaisesRBRefactoringError
	self shouldFail: (RBRemoveClassRefactoring classNames: #(#RBBasicLintRuleTest ))