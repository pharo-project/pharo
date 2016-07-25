private
shouldFail: aRefactoring 
	self proceedThroughWarning: [
		self 
			should: [ self executeRefactoring: aRefactoring ]
			raise: RBRefactoringError ]