private-method
privateRemoveMethodsFor: aCollection
	| selectors class |
	selectors := aCollection collect: #selector.
	class := aCollection first origin.
	^ RBRemoveMethodRefactoring
		model: environment
		removeMethods: selectors
		from: class