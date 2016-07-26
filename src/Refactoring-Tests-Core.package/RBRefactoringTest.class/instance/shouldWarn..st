private
shouldWarn: aRefactoring 
	self 
		should: [ self executeRefactoring: aRefactoring ]
		raise: RBRefactoringWarning