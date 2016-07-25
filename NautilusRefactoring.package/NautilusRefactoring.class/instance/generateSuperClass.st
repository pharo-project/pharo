class
generateSuperClass
	"Execute the refactoring of the receiver."

	| refactoring |
	refactoring := self privateGenerateSuperClassFor: self model selectedClass theNonMetaClass.
	self performRefactoring: refactoring.