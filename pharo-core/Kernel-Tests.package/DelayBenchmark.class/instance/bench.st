benchmarks
bench
	"DelayBenchmark runAll."
	"DelayBenchmark run."
	| sampleSetSeeds trialOfNumberConcurrent completed |
	sampleSetSeeds := #( 42 "5975 13746 22634 44022" ). "uncomment items to get a better average"
	trialOfNumberConcurrent := #( 1000 2000 3000 4000 5000 6000 7000 8000 9000 10000 ).
	completed := Semaphore new.

Transcript cr; crShow: 'Benchmarking ' , Delay delaySchedulerClass printString.
[ 	trialOfNumberConcurrent do: [ :numberConcurrent |   
		Transcript crShow:  '#ConcurrentDelays: ' , numberConcurrent printString ; space.
		EnterCount := ExitCount := 0.
		sampleSetSeeds do: [ :seed |  
			Transcript show: '.'.
			self 
				trialConcurrent: numberConcurrent 
				priority: Processor userSchedulingPriority + 1
				maxDuration: 50 
				fromSeed: seed.
			].
		Transcript space ;
			show: 'EnterCount: ' , (EnterCount // (sampleSetSeeds size)) printString ; space ;
			show: 'ExitCount: ' , (ExitCount // (sampleSetSeeds size)) printString .
		World doOneCycle. "since we are a higher priority would prevent transcript showing"
		]. 
		completed signal.
] forkAt: Processor userSchedulingPriority + 2. "To avoid UI loop influencing result"

completed wait.


 
		