private building
variableValuePairsFor: anObject

	^ (self basicVariableValuePairsFor: anObject) asOrderedCollection 
		addFirst: 'self' -> anObject; 
		yourself