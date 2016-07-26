private-class
privateInsertSubclassFor: class
	^ RBAddClassRefactoring
		model: environment
		addClass: ( self request: 'Enter new subclass name :')
		superclass: class
		subclasses: class subclasses
		category: class category