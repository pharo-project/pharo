preconditions
checkBrowseOccurrences: anCollectionOfOccurrences
	anCollectionOfOccurrences size = 1
		ifTrue: [ ^ self checkBrowseOccurrenceOf: anCollectionOfOccurrences first key in: anCollectionOfOccurrences first value ]
		ifFalse: [ ^ self checkBrowseAllOccurences: anCollectionOfOccurrences ]