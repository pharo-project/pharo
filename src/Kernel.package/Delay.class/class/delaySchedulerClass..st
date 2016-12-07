settings
delaySchedulerClass: aSchedulerClass 
	| newScheduler |
	self delaySchedulerClass = aSchedulerClass ifTrue:[ ^self ].
	newScheduler := aSchedulerClass new.
	(newScheduler respondsTo: #startTimerEventLoop) 
		ifFalse: [ self error: 'New delay scheduler must respond to #startTimerEventLoop' ].
	Scheduler stopTimerEventLoop.
	Scheduler := newScheduler.
	Scheduler startTimerEventLoop.
	self inform: 'Delay scheduler set to ' , aSchedulerClass printString.
	
	