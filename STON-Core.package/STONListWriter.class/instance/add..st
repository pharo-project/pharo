accessing
add: anObject
	first ifTrue: [ first := false ] ifFalse: [ writer listElementSeparator ].
	writer nextPut: anObject