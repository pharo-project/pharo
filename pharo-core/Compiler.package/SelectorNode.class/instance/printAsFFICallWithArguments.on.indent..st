printing
printAsFFICallWithArguments: aSequence on: aStream indent: level
	aStream nextPutAll: (key copyUpTo: $)).
	aSequence
		do: [:arg| arg printOn: aStream indent: level]
		separatedBy: [aStream nextPutAll: ', '].
	aStream nextPut: $)