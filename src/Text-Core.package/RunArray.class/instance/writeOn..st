printing
writeOn: aStream

	aStream nextWordPut: runs size.
	1 to: runs size do:
		[:x |
		aStream nextWordPut: (runs at: x).
		aStream nextWordPut: (values at: x)]