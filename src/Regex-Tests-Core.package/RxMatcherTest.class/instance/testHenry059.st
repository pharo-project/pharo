testing-henry
testHenry059
	self runRegex: #('[[:alpha:]]+'
		' 123foo3 ' true (1 'foo')
		'123foo' true (1 'foo')
		'foo1b' true (1 'foo'))