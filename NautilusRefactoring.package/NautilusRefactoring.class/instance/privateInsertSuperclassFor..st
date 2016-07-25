private-class
privateInsertSuperclassFor: class
	^ RBAddClassRefactoring
		model: environment
		addClass: (self request: 'Enter new superclass name :')
		superclass: class superclass
		subclasses: class asOrderedCollection
		category: class category