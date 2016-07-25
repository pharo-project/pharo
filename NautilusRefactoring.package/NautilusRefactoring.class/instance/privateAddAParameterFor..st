private-method
privateAddAParameterFor: aMethod
	| initialAnswer oldSelector |
	oldSelector := aMethod selector.
	initialAnswer := oldSelector numArgs = 0
		ifTrue: [ oldSelector , ':' ]
		ifFalse: [ oldSelector ].
	^ RBAddParameterRefactoring
		model: environment
		addParameterToMethod: aMethod selector
		in: aMethod origin
		newSelector: (self request: 'Enter new selector:' initialAnswer: initialAnswer) asSymbol
		initializer: (self request: 'Enter default value for parameter:' initialAnswer: 'nil')