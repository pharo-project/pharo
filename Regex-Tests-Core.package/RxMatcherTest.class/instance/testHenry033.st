testing-henry
testHenry033
	self runRegex: #('.+\<foo'
		'foo' false nil
		'ab foo' true (1 'ab foo')
		'abfoo' false nil)