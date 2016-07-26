initialization
initialize
	super initialize.
	mutex := Mutex new.
	queue := WaitfreeQueue new.
	permission := true.
	preSuspendingAction := nil.