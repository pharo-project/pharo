testing-henry
testHenry006
	self runRegex: #('^abc$'
		'abc' true (1 'abc')
		'abcc' false nil
		'aabc' false nil)