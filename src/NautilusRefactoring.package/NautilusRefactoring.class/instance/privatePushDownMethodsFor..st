private-method
privatePushDownMethodsFor: aCollection
	| selectors class |
	selectors := aCollection collect: #selector.
	class := aCollection first methodClass.
	^ RBPushDownMethodRefactoring
		model: environment
		pushDown: selectors
		from: class