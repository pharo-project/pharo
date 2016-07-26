testing
testRegex001
	self runRegex: #('^.*$' 
		'' true (1 '')
		'a' true (1 'a')
		'abc' true (1 'abc'))