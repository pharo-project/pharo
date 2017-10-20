testing
testWikipedia1
	self 
		assert: (MD5 hashMessage: 'The quick brown fox jumps over the lazy dog') hex 
		equals: '9e107d9d372bb6826bd81d3542a419d6'