testing-henry
testHenry047
	self runRegex: #('a[^-b]c'
		'adc' true (1 'adc')
		'a-c' false nil)