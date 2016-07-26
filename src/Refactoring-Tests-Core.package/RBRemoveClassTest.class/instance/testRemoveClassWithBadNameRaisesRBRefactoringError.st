failure tests
testRemoveClassWithBadNameRaisesRBRefactoringError 
	self 
		shouldFail: (RBRemoveClassRefactoring classNames: #(#RecursiveSelfRule))