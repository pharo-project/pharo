tests
testRaisesRBRefactoringErrorWhenRemovingNonEmptySuperclass
	self shouldFail: (RBRemoveClassRefactoring classNames: (Array with: ('RBFoo' , 'LintRuleTest') asSymbol))