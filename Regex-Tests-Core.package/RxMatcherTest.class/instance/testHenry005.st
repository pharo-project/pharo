testing-henry
testHenry005
	self runRegex: #('ab?bc'
		'abbc' true (1 'abbc')
		'abc' true (1 'abc')
		'abbbbc' false nil
		'abc' true (1 'abc'))