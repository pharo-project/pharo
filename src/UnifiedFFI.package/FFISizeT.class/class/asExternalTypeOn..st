converting
asExternalTypeOn: generator
	self pointerSize = 4 ifTrue: [ ^ generator resolveType: #uint32 ].
	self pointerSize = 8 ifTrue: [ ^ generator resolveType: #uint64 ].
	self error: 'no clue'