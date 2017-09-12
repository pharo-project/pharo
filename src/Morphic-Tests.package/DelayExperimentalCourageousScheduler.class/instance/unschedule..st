low priority processes
unschedule: aDelay

	"Depends on... semantics of bytecode inlining and cooperative multitasking  
	 such that interuption cannot occur between the assignment and the signal.
	 The #signal invokes the highest priority thread to consume finishedDelay, 
	 so no other thread has a chance to overwrite our value."	

	finishedDelay := aDelay. 
	timingSemaphore signal.

