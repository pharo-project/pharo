private-method
privatePullUpMethodsFor: aCollection
	| selectors class |
	selectors := aCollection collect: #selector.
	class := aCollection first methodClass.
	^ RBPullUpMethodRefactoring model: environment pullUp: selectors from: class