mutual exclusion
critical: aBlock
	"Evaluate aBlock protected by the receiver."
	| activeProcess |
	activeProcess := Processor activeProcess.
	activeProcess == owner ifTrue:[^aBlock value].
	^semaphore critical:[
		owner := activeProcess.
		aBlock ensure:[owner := nil]].