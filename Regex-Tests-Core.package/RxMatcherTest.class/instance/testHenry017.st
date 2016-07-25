testing-henry
testHenry017
	self runRegex: #('\W+'
		'foo!@#$bar' true (1 '!@#$')
		'foobar' false nil)