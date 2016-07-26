testing-henry
testHenry025
	self runRegex: #('\D+'
		'1234 abc 456' true (1 ' abc ')
		'1234567890' false nil)