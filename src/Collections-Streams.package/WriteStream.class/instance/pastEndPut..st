private
pastEndPut: anObject
	"Grow the collection by doubling the size, but keeping the growth between 20 and 1000000.
	Then put <anObject> at the current write position."

	collection := collection grownBy: ((collection size max: 20) min: 1000000).
	writeLimit := collection size.
	collection at: (position := position + 1) put: anObject.
	^ anObject