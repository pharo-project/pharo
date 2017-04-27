scheduling
newProcess
	"Answer a Process running the code in the receiver. The process is not 
	scheduled.
	IMPORTANT! Debug stepping this deep infrastructure may lock your Image
  	If you are not sure what you are doing, close the debugger now."
	<primitive: 19> "Simulation guard"
	^Process
		forContext: 
			[self value.
			Processor terminateActive] asContext
		priority: Processor activePriority