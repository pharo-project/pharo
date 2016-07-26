tests
testRaisesRBRefactoringErrorWhenRemovingClassWithSubclasses
	self shouldFail: (RBRemoveClassRefactoring classNames: (Array with: ('RBFoo' , 'LintRuleTest1') asSymbol))