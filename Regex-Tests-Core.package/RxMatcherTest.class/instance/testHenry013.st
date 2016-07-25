testing-henry
testHenry013
	self runRegex: #('.a.*'
		'1234abc' true (1 '4abc')
		'abcd' false nil)