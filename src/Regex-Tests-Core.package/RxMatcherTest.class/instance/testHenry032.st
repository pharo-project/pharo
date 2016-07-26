testing-henry
testHenry032
	self runRegex: #('\<foo.*'
		'foo' true nil
		'foobar' true nil
		'qfoobarq foonix' true (1 'foonix')
		' foo' true nil
		' 12foo' false nil
		'barfoo' false nil)