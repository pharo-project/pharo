testing-henry
testHenry003
	self runRegex: #('ab*bc'
		'abc' true (1 'abc')
		'abbc' true (1 'abbc')
		'abbbbc' true (1 'abbbbc'))