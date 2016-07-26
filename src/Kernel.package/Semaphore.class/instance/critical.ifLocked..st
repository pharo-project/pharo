mutual exclusion
critical: mutuallyExcludedBlock ifLocked: alternativeBlock
	"Evaluate mutuallyExcludedBlock only if the receiver is not currently in 
	the process of running the critical: message. If the receiver is, evaluate 
	mutuallyExcludedBlock after the other critical: message is finished."
	
	"Note: The following is tricky and depends on the fact that the VM will not switch between 
processes while executing byte codes (process switches happen only in real sends). The following 
test is written carefully so that it will result in bytecodes only."

	excessSignals == 0 
		ifTrue: 
			[ "If we come here, then the semaphore was locked when the test executed. 
			Evaluate the alternative block and answer its result."
			^alternativeBlock value ].
	^self critical: mutuallyExcludedBlock