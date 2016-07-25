testing-henry
testHenry079
	self runRegex: #('a\(*b'
		'ab' true (1 'ab')
		'a((b' true (1 'a((b'))