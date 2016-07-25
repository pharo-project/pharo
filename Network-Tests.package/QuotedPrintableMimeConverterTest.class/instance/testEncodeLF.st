testing
testEncodeLF

	self 
		assert: (self encode: 'This is the first line', String lf, 'and this is the second')
		equals: 'This is the first line=0D=0Aand this is the second'