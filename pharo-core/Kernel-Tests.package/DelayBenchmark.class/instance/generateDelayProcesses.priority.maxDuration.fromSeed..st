support
generateDelayProcesses: numberConcurrent priority: priority maxDuration: maxDuration fromSeed: seed 
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
				[	| delay | 
					delay := Delay forMilliseconds: 0.
					[	sampleIndex := (sampleIndex rem: sampleDurations size) + 1.
						delay setDelay: (sampleDurations at: sampleIndex).
						EnterCount := EnterCount + 1. 	
						delay wait.
						ExitCount := ExitCount + 1.
						Running.  "Repeat until not running." 
					] whileTrue.
 				] newProcess priority: priority
			]


