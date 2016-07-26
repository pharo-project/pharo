process state change
suspendFirstAt: aPriority ifNone: noneBlock 
	"Suspend the first Process that is waiting to run with priority aPriority. If 
	no Process is waiting, evaluate the argument, noneBlock."

	| aList |
	aList := quiescentProcessLists at: aPriority.
	aList isEmpty
		ifTrue: [^noneBlock value]
		ifFalse: [^aList first suspend]