evaluating
collate: anObject1 with: anObject2
	"Do a three-way comparison between the anObject1 and anObject2 according to the given direction,
	which can be:
	1 - specifies native comparison logic.
	-1 - specifies reversed comparison logic. 
	Subclases implement native comparison logic in the method #threeWayCompare:with:. 
	Look at it for details"

	^(self threeWayCompare: anObject1 with: anObject2) * direction