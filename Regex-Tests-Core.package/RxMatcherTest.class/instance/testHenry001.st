testing-henry
testHenry001
	self runRegex: #('abc'
		'abc' true (1 'abc')
		'xbc' false nil
		'axc' false nil
		'abx' false nil
		'xabcy' true (1 'abc')
		'ababc' true (1 'abc'))