class
removeClasses: aCollection
	"Execute the refactoring of the receiver."

	| refactoring |
	refactoring := self classRemovalRefactoringObjectFor: aCollection.
	refactoring model environment: self model browsedEnvironment.
	self performRefactoring: refactoring.
	^ true