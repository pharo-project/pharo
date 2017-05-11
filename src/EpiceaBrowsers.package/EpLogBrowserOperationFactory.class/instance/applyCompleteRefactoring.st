public
applyCompleteRefactoring

	| refactoringEntry refactoring |
	refactoringEntry := self entries first entries first. "TODO: only first?"
	refactoring := refactoringEntry content asRBRefactoring.
	
	self applyRBRefactoring: refactoring
