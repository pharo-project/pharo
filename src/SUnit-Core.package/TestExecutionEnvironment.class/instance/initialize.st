initialization
initialize
	super initialize.
	
	forkedProcesses := OrderedCollection new.
	failedProcesses := Dictionary new.
	testCompleted := false