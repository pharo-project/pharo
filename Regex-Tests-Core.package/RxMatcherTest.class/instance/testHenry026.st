testing-henry
testHenry026
	self runRegex: #('(f|o)+\b'
		'foo' true (1 'foo')
		' foo ' true (1 'foo'))