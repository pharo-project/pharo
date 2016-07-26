transforming
moveMethod
	| variable refactoring |
	variable := self selectVariableToMoveMethodTo: extractedMethodSelector
				class: class.
	variable isNil ifTrue: [self refactoringFailure: 'Did not extract method'].
	refactoring := RBMoveMethodRefactoring 
				model: self model
				selector: extractedMethodSelector
				class: class
				variable: variable.
	self performComponentRefactoring: refactoring