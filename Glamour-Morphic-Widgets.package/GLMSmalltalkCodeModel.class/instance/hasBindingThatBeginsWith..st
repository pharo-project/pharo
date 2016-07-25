accessing
hasBindingThatBeginsWith: aString
	^ self variableBindings keys anySatisfy: [:each | each beginsWith: aString]