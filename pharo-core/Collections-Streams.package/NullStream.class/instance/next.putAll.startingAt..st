writing
next: anInteger putAll: aCollection startingAt: startIndex
	"Store the next anInteger elements from the given collection."

	position := position + anInteger.
	lastElement := aCollection at: anInteger + startIndex - 1.
	^aCollection