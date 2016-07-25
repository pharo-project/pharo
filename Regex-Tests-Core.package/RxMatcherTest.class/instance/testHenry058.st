testing-henry
testHenry058
	self runRegex: #('[%&[:alnum:]]+'
		'foo%3' true (1 'foo%3')
		'foo34&rt4$57a' true (1 'foo34&rt4')
		'!@#$' false nil)