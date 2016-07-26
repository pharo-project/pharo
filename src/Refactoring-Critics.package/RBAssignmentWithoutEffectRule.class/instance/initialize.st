initialization
initialize
	super initialize.
	self matcher 
		matches: '`var := `var'
		do: [ :node :answer | node ]