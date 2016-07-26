private-class
classRemovalRefactoringObjectFor: aCollection
	^ RBRemoveClassRefactoring 
		model: environment 
		classNames: (aCollection collect: [:e | e theNonMetaClass name ])