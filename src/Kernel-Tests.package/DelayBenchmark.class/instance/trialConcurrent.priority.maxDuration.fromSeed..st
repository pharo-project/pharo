support
trialConcurrent: numberConcurrentDelays priority: priority maxDuration: maxDuration fromSeed: seed 

	| delayProcesses |
	delayProcesses := self 
		generateDelayProcesses: numberConcurrentDelays 
		priority: priority 
		maxDuration: maxDuration 
		fromSeed: seed.
	
	Running := false.			
	
	[	Running := true. 
		delayProcesses do: [  :process | process resume ]. "Start counting delays"
		(Delay forSeconds: 1) wait. "Count the number of delays in one second"
	] ensure: [ Running := false ].  "All processes should exit cleanly here."
	(Delay forSeconds: 1) wait.	 "but give some time for clean exit"	
	delayProcesses do: [  :process | process terminate ]. "and cleanup any deadlocks."
	
	Smalltalk garbageCollect. Smalltalk garbageCollect. Smalltalk garbageCollect. 
