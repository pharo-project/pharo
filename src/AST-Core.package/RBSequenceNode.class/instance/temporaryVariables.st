accessing
temporaryVariables
	^(super temporaryVariables asOrderedCollection)
		addAll: self temporaryNames;
		yourself