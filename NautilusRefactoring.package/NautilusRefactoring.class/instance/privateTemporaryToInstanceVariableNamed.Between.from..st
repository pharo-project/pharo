private-source
privateTemporaryToInstanceVariableNamed: name Between: anInterval from: aMethod
	^ RBTemporaryToInstanceVariableRefactoring
		model: environment
		class: aMethod methodClass
		selector: aMethod selector
		variable: name