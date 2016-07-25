testing-henry
testHenry031
	self runRegex: #('\>.+abc'
		' abcde fg' false nil
		'foo abcde' true (1 ' abc')
		'abcde' false nil)