testing-henry
testHenry022
	self runRegex: #('a\d+c'
		'a0123456789c' true (1 'a0123456789c')
		'a12b34c' false nil)