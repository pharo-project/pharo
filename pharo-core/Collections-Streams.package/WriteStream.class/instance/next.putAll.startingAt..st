accessing
next: anInteger putAll: aCollection startingAt: startIndex
	"Store the next anInteger elements from the given collection."

	| newEnd numPut |
	collection class == aCollection class ifFalse:
		[^ super next: anInteger putAll: aCollection startingAt: startIndex ].

	numPut := anInteger min: (aCollection size - startIndex + 1).
	newEnd := position + numPut.
	newEnd > writeLimit ifTrue:
		[^ super next: anInteger putAll: aCollection startingAt: startIndex "Trigger normal pastEndPut: logic"].

	collection replaceFrom: position+1 to: newEnd with: aCollection startingAt: startIndex.
	position := newEnd.
