tests
testReadFrom

	| m |
	m := Month readFrom: 'July 1998' readStream.
	self 
		assert: m = month