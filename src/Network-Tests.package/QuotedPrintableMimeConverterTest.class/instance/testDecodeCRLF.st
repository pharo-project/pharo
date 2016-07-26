testing
testDecodeCRLF

	self 
		assert: (self decode: 'This is the first line=0D=0A and this is the second')
		equals: 'This is the first line
 and this is the second'