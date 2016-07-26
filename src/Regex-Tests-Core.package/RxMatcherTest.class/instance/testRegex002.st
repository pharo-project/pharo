testing
testRegex002
	self runRegex: #('a\w+c'
		' abb_bbc ' true (1 'abb_bbc')
		'abb-bc' false nil)