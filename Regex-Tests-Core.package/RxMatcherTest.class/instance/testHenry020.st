testing-henry
testHenry020
	self runRegex: #('a\S*c'
		'aqwertyc' true (1 'aqwertyc')
		'ab c' false nil)