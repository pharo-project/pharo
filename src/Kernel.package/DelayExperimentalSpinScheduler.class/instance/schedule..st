low priority processes
schedule: aDelay

	"Depends on the bytecode inlining and cooperative multitasking semantics 
	 such that interuption can occur between the test and the assignment."
	
	aDelay schedulerBeingWaitedOn ifTrue: [^self error: 'This Delay has already been scheduled.'].
	[ 	scheduledDelay == nil ifTrue: [ 
				scheduledDelay := aDelay. 
				timingSemaphore signal.
				^self
				].
		true.
	] whileTrue.	
