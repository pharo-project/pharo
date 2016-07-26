testing-henry
testHenry028
	self runRegex: #('(f|o)+\B'
		'quuxfoobar' true (1 'foo')
		'quuxfoo ' true (1 'fo'))