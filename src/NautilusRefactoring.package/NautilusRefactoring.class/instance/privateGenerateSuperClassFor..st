private-class
privateGenerateSuperClassFor: class
	| superclassName subclasses |
	superclassName := self request: 'Enter new superclass name:'.
	subclasses := self chooseMultipleFrom: class subclasses title: 'Select subclasses of ' , superclassName , ':'.
	subclasses ifNil: [ RefactoringAborted signal ].
	^ RBChildrenToSiblingsRefactoring
		model: environment
		name: superclassName
		class: class
		subclasses: subclasses