initialization
initialize

	files := Array new: 2.
	
	readOnlyQueue := SharedQueue new.
	flushChanges := true