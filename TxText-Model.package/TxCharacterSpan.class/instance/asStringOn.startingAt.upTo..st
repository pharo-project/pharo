printing
asStringOn: aStream startingAt: startIndex upTo: endIndex
	aStream nextPutAll: (characters copyFrom: startIndex + 1 to: endIndex).