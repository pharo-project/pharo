cleaning
cleanupCategories: aWorkingCopy
	aWorkingCopy packageSet systemCategories do: [ :category |
		(Smalltalk organization classesInCategory: category) isEmpty
			ifTrue: [ Smalltalk organization removeSystemCategory: category ] ]