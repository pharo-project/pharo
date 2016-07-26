private
listAtCategoryNumber: anInteger 
	"Answer the array of elements stored at the position indexed by anInteger.  Answer nil if anInteger is larger than the number of categories."

	| firstIndex lastIndex |
	(anInteger < 1 or: [anInteger > categoryStops size])
		ifTrue: [^ nil].
	firstIndex := self firstIndexOfCategoryNumber: anInteger.
	lastIndex :=  self lastIndexOfCategoryNumber: anInteger.
	^elementArray copyFrom: firstIndex to: lastIndex