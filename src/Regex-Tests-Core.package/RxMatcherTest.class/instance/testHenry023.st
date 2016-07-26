testing-henry
testHenry023
	self runRegex: #('\d+'
		'foo@#$%123ASD #$$%^&' true (1 '123')
		'foo!@#$asdfl;' false nil)