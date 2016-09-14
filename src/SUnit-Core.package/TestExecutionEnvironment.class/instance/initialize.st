initialization
initialize
	super initialize.
	
	forkedProcesses := WeakSet new.
	failedProcesses := Dictionary new.
	testCompleted := false