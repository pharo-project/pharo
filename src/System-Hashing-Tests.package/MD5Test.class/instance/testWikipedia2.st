testing
testWikipedia2
	self 
		assert: (MD5 hashMessage: 'The quick brown fox jumps over the lazy dog.') hex 
		equals: 'e4d909c290d0fb1ca068ffaddf22cbd0'