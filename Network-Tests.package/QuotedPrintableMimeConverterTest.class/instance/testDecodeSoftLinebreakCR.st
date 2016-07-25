testing
testDecodeSoftLinebreakCR

	self 
		assert: (self decode: 'This is text may not be=', String cr, ' decoded as two lines')
		equals: 'This is text may not be decoded as two lines'