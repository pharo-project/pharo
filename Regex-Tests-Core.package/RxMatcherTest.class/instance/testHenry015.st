testing-henry
testHenry015
	self runRegex: #('\w+'
		'  	foobar	quux' true (1 'foobar')
		' 	~!@#$%^&*()-+=\|/?.>,<' false nil)