private-accessing
possibleSetterSelectors
	^self methodsReferencingVariable select: [:each | each numArgs == 1]