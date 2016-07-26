process state change
suspendFirstAt: aPriority 
	"Suspend the first Process that is waiting to run with priority aPriority."

	^self suspendFirstAt: aPriority
		  ifNone: [self error: 'No Process to suspend']