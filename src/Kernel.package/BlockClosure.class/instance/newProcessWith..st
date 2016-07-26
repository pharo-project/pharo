scheduling
newProcessWith: anArray 
	"Answer a Process running the code in the receiver. The receiver's block 
	arguments are bound to the contents of the argument, anArray. The 
	process is not scheduled."
	<primitive: 19> "Simulation guard"
	^Process
		forContext: 
			[self valueWithArguments: anArray.
			Processor terminateActive] asContext
		priority: Processor activePriority