positioning
positionOfSubCollection: subCollection
	"Return a position such that that element at the new position equals the first element of sub, and the next elements equal the rest of the elements of sub. Begin the search at the current position.
	If no such match is found, answer 0."

	^self positionOfSubCollection: subCollection ifAbsent: [0]