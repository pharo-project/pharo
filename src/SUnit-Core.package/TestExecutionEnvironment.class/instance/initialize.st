initialization
initialize
	super initialize.
	
	forkedProcesses := WeakSet new.
	failedProcesses := OrderedIdentityDictionary new.
	testCompleted := false