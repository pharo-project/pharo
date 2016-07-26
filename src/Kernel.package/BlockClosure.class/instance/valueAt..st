evaluating
valueAt: blockPriority 
	"Evaluate the receiver (block), with another priority as the actual one 
	and restore it afterwards. The caller should be careful with using 
	higher priorities."
	| activeProcess result outsidePriority |
	activeProcess := Processor activeProcess.
	outsidePriority := activeProcess priority.
	activeProcess priority: blockPriority.
	result := self ensure: [activeProcess priority: outsidePriority].
	"Yield after restoring lower priority to give the preempted processes a  
	chance to run."
	blockPriority > outsidePriority
		ifTrue: [Processor yield].
	^ result