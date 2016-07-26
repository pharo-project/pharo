accessing
preemptedProcess
	"Return the process that the currently active process just preempted."
	| list |
	activeProcess priority to: 1 by: -1 do:[:priority|
		list := quiescentProcessLists at: priority.
		list isEmpty ifFalse:[^list last].
	].
	^nil

	"Processor preemptedProcess"