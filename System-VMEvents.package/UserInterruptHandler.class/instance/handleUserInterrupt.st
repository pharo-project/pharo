private
handleUserInterrupt
	"This will be called from the event-fetcher process. 
	Assume no system-vital processes have a lower priority than this, and are thus ok to interrupt"
	UserInterruptHandler cmdDotEnabled ifFalse: [ ^ self ]. 

	[
		self processToInterrupt
			ifNotNil: [ :proc | proc debugWithTitle: 'User Interrupt' ]
			ifNil: [ UIManager default inform: 'Process not interruptable' ]
	
	"fork exceptions, we don't want interrupt handler to die"
	] on: Error fork: [:ex | ex pass].