running
setUp
	super setUp.
	
	forkedProcesses := OrderedCollection new.
	mutex := Mutex new.
	