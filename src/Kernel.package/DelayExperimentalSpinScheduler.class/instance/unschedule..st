low priority processes
unschedule: aDelay
	
	[	finishedDelay == nil ifTrue: [ 
				finishedDelay := aDelay. "...and this assignment" 
				timingSemaphore signal.
				^true.
				].
		true.
	] whileTrue.	
