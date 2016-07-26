accessing
findAnySubstring: aCollection startingAt: start 
	"Answer the index where an element of aCollection begins. If none are found, answer size + 1. aCollection is an Array of Strings or Characters."

	^aCollection inject: 1 + self size into: [:min :searchTerm |
		| ind |
		ind := searchTerm isCharacter 
			ifTrue: [self indexOf: searchTerm startingAt: start ifAbsent: [min]]
			ifFalse: [self indexOfSubCollection: searchTerm startingAt: start ifAbsent: [min]].
		min min: ind]