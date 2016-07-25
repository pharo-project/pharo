testing-henry
testHenry030
	self runRegex: #('fooa\>.*'
		'fooa ' true nil
		'fooa123' false nil
		'fooa bar' true nil
		'fooa' true nil
		'fooargh' false nil)