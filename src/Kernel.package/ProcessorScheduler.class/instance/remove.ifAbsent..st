removing
remove: aProcess ifAbsent: aBlock 
	"Remove aProcess from the list on which it is waiting for the processor 
	and answer aProcess. If it is not waiting, evaluate aBlock."

	(quiescentProcessLists at: aProcess priority)
		remove: aProcess ifAbsent: aBlock.
	^aProcess