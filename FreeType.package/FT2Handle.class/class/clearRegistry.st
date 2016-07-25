system startup
clearRegistry
	self flag: #todo. "This is a WORKAROUND to ensure all handle instances are niled on startup 
	(they are external memory, so they need to be re-created, and for some reason not all handles 
	 are registered to clean up... not good)"
	self allSubInstancesDo: [ :each | each beNull ].
	self registry
		"do: [ :each | each beNull ];"
		removeAll.