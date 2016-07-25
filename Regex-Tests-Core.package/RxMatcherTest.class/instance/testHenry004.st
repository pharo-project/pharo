testing-henry
testHenry004
	self runRegex: #('ab+bc'	
		'abbc' true (1 'abbc')
		'abc' false nil
		'abq' false nil
		'abbbbc' true (1 'abbbbc'))