testing-henry
testHenry016
	self runRegex: #('a\W+c'
		'a   c' true (1 'a   c')
		'a bc' false nil)