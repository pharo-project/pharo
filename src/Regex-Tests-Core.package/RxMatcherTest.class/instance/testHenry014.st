testing-henry
testHenry014
	self runRegex: #('a\w+c'
		' abbbbc ' true (1 'abbbbc')
		'abb bc' false nil)