testing
isEmptyCategoryNumber: anInteger

	| firstIndex lastIndex |
	(anInteger < 1 or: [anInteger > categoryStops size])
		ifTrue: [^ true].
	firstIndex := self firstIndexOfCategoryNumber: anInteger.
	lastIndex :=  self lastIndexOfCategoryNumber: anInteger.
	^ firstIndex > lastIndex