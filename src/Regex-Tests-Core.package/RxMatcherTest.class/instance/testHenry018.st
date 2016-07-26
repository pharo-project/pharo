testing-henry
testHenry018
	self runRegex: #('a\s*c'
		'a   c' true (1 'a   c')
		'a bc' false nil)