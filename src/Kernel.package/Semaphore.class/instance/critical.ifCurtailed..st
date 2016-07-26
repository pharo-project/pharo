mutual exclusion
critical: mutuallyExcludedBlock ifCurtailed: terminationBlock
	"Evaluate mutuallyExcludedBlock only if the receiver is not currently in 
	the process of running the critical: message. If the receiver is, evaluate 
	mutuallyExcludedBlock after the other critical: message is finished."
	^self critical:[mutuallyExcludedBlock ifCurtailed: terminationBlock]
