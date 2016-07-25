testing-henry
testHenry048
	self runRegex: #('a[^]b]c'
		'a]c' false nil
		'adc' true (1 'adc'))