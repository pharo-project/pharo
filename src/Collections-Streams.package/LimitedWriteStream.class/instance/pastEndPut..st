private
pastEndPut: anObject
	collection size >= limit ifTrue: [limitBlock value].  "Exceptional return"
	^ super pastEndPut: anObject