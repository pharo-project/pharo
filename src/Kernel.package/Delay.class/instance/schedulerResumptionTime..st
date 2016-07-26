private - scheduler
schedulerResumptionTime: anInteger
	"Private! Should only be called by a subclass of DelayScheduler.
	Set the value of the system's clock at which the receiver's suspended Process will resumed.  The clock semantics depends on the scheduler used e.g. millisecond/microsecond."
	
	schedulerResumptionTime := anInteger
	
