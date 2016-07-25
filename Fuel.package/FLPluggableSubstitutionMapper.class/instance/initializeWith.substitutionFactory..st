initialize-release
initializeWith: aCondition substitutionFactory: aBlock

	self initialize.
	condition := aCondition.
	substitutionFactory := aBlock.