testing
testDecodeSoftLinebreakLF

	self 
		assert: (self decode: 'This is text may not be=', String lf, ' decoded as two lines')
		equals: 'This is text may not be decoded as two lines'