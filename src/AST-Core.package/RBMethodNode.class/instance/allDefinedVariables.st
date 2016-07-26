accessing
allDefinedVariables
	^(self argumentNames asOrderedCollection)
		addAll: super allDefinedVariables;
		yourself