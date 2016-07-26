class
generateAccessors

	| refactorings |
	refactorings := self privateGenerateAccessorsFor: self model selectedClass.
	self performRefactorings: refactorings