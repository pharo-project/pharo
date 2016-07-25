testing-henry
testHenry021
	self runRegex: #('\S+'
		'     	asdf		' true (1 'asdf')
		' 	
			' false nil)