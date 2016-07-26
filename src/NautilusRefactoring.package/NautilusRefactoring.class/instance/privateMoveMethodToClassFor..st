private-method
privateMoveMethodToClassFor: aMethod
	| class |
	class := (SearchFacade classSearchInEnvironment: Smalltalk) chooseFromOwner: World.
	class ifNil: [ RefactoringAborted signal ].
	^ RBMoveMethodToClassRefactoring
		model: environment
		method: aMethod
		class: class