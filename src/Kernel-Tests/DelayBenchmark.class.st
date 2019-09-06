"
The delay scheduling loop is THE highest priority code which is run in Pharo, in other words it is time-critical. The speed of this code is critical for accurate responses, it is critical for network services, it affects every last part of the system.

DelayBenchmark provides a means for evaluating modifications.  This is fairly basic, providing only Transcript output of results.

Over a number of trials the amount of concurrent delays is increased.  The durations are randomly pre-generated into an array to avoid that overhead in the trials.  A fixed seed is used to try and improve comparisons between runs. Uncomment the additional seeds to provide better averaging of results.
"
Class {
	#name : #DelayBenchmark,
	#superclass : #Object,
	#classVars : [
		'EnterCount',
		'ExitCount',
		'Running'
	],
	#category : #'Kernel-Tests-Delays'
}

{ #category : #action }
DelayBenchmark class >> run [
	"DelayBenchmark run"
	self new bench.
]

{ #category : #action }
DelayBenchmark class >> runAll [
	"DelayBenchmark runAll"

	DelayNullScheduler allSubclasses
		do: [ :delaySchedulerClass | self new bench: delaySchedulerClass ]
]

{ #category : #benchmarks }
DelayBenchmark >> bench [
	"DelayBenchmark runAll."

	"DelayBenchmark run."

	| sampleSetSeeds trialOfNumberConcurrent completed |
	sampleSetSeeds := #(42).	"5975 13746 22634 44022"	"uncomment items to get a better average"
	trialOfNumberConcurrent := #(1000 2000 3000 4000 5000 6000 7000 8000 9000 10000).
	completed := Semaphore new.
	self
		crTrace: String cr , 'Benchmarking ' , Delay delaySchedulerClass printString.
	[ trialOfNumberConcurrent
		do: [ :numberConcurrent | 
			self
				crTrace: '#ConcurrentDelays: ' , numberConcurrent printString , String space.
			EnterCount := ExitCount := 0.
			sampleSetSeeds
				do: [ :seed | 
					self trace: '.'.
					self
						trialConcurrent: numberConcurrent
						priority: Processor userSchedulingPriority + 1
						maxDuration: 50
						fromSeed: seed ].
			self trace: String space.
			self
				trace:
					'EnterCount: ' , (EnterCount // sampleSetSeeds size) printString
						, String space.
			self
				trace: 'ExitCount: ' , (ExitCount // sampleSetSeeds size) printString.
			self currentWorld doOneCycle	"since we are a higher priority would prevent transcript showing" ].
	completed signal ] forkAt: Processor userSchedulingPriority + 2.	"To avoid UI loop influencing result"
	completed wait
]

{ #category : #benchmarks }
DelayBenchmark >> bench: aDelaySchedulerClass [
	| schedulerClassToRestore |
	
	self assert: (aDelaySchedulerClass inheritsFrom: DelayNullScheduler).
	schedulerClassToRestore := Delay delaySchedulerClass.
	
	[	Delay delaySchedulerClass: aDelaySchedulerClass.
		self bench.
	] ensure: [ Delay delaySchedulerClass: schedulerClassToRestore].
]

{ #category : #support }
DelayBenchmark >> generateDelayProcesses: numberConcurrent priority: priority maxDuration: maxDuration fromSeed: seed [ 
	| randomGenerator sampleDurations sampleIndex | 
	"Pre-generate a series of random durations to remove such time for such generation from trial" 
	"Seed to help reproducability between trials."
	randomGenerator := Random seed: seed.
	sampleDurations := Array new: 1000.  
	1 to: sampleDurations size do:
	[  	:index | 
		sampleDurations at: index put: (randomGenerator nextInt: maxDuration).
	].
	sampleIndex := 0.

	"Avoid instantiating new Delay objects inside the inner whileTrue loop, to eliminate such time from trial. 
	Reuse Delay object with modified duration each loop."
    ^ (1 to: numberConcurrent) collect: 
			[ 	:each | 
				[					
					[	| delay | 	
						sampleIndex := (sampleIndex rem: sampleDurations size) + 1.
						delay := Delay forMilliseconds: (sampleDurations at: sampleIndex).
						EnterCount := EnterCount + 1. 	
						delay wait.
						ExitCount := ExitCount + 1.
						Running.  "Repeat until not running." 
					] whileTrue.
 				] newProcess priority: priority
			]



]

{ #category : #support }
DelayBenchmark >> trialConcurrent: numberConcurrentDelays priority: priority maxDuration: maxDuration fromSeed: seed [ 

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

]
