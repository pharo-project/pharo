instance creation
streamedRepresentationOf: anObject

	| file |
	file := (RWBinaryOrTextStream on: (ByteArray new: 5000)).
	file binary.
	(self on: file) nextPut: anObject.
	^file contents