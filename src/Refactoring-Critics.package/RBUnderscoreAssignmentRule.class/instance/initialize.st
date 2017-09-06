initialization
initialize
	super initialize.
	self 
		replace: '`var := ``@object'
		with:    '`var := ``@object'
		"yes, they are the same"
		"see afterCheck"