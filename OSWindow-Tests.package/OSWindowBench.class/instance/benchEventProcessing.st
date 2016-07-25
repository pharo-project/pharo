benchmarking
benchEventProcessing
	"self new benchEventProcessing"
	| window sema |
	
	window := World osWindow.
	sema := Semaphore new.
	[ 
"		TimeProfiler spyAllOn:[ sema wait ]. 
"		AndreasSystemProfiler spyOn: [ sema wait ].
	] forkAt: Processor userInterruptPriority.
	
	[  
		20 timesRepeat: [  
		1 to: 500 do: [ :i | | event |
		
		
			event := OSMouseMoveEvent for: window.
			event position: i @ 100;
				delta: 1 @ 0.

			event deliver.
		].
	(Delay forMilliseconds: 20) wait.
	].
		sema signal.
	] forkAt: 60.

	
