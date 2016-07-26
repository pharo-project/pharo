printing
printOn: aStream indent: level

	aStream nextPut: ${.
	1 to: elements size do: 
		[:i | (elements at: i) printOn: aStream indent: level.
		i < elements size ifTrue: [aStream nextPutAll: '. ']].
	aStream nextPut: $}