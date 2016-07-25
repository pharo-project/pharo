accessing
allDefinedVariables
	^(self temporaryNames asOrderedCollection)
		addAll: super allDefinedVariables;
		yourself