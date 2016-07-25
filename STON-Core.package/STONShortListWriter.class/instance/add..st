accessing
add: anObject
	first ifTrue: [ first := false ] ifFalse: [ writer shortListElementSeparator ].
	writer nextPut: anObject