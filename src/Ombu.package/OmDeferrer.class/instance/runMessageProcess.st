scheduling
runMessageProcess
	process := [ 
		"Loop until enough time passed since last #schedule"
		[ DateAndTime now < (lastSchedule + duration) ]
			whileTrue: [ duration wait ].
			
		"Execute deferred action (if not flushed before) and clean up"
		self flush.
	] fork