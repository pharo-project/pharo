low priority processes
schedule: aDelay

	"Depends on... semantics of bytecode inlining and cooperative multitasking  
	 such that interuption cannot occur between the assignment and the signal.
	 The #signal invokes the highest priority thread to consume scheduledDelay, 
	 so no other thread has a chance to overwrite our value."

	aDelay schedulerBeingWaitedOn ifTrue: [^self error: 'This Delay has already been scheduled.'].
	
	scheduledDelay := aDelay. 
	timingSemaphore signal.
