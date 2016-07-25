private-accessing
possibleGetterSelectors
	^self methodsReferencingVariable select: [:each | each numArgs == 0]