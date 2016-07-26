synchronization
critical: aBlock
	"Critical section.
	Executes aBlock as a critical section. At any time, only one process can be executing code 
	in a critical section.
	NOTE: All the following synchronization operations are only valid inside the critical section 
	of the monitor!"

	^[
	self enter.
	aBlock value]
		ensure: [self exit].