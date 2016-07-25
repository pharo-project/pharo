examples
exampleDebug
	"Job exampleDebug"
	| aJob |
	aJob := [ :job| 
		"Set some job properties"
		job 
			title: 'aTitle';
			max: 10.
		1 to: 10 do: [ :i|
			job title: 'Fib ', i asString.
			"do some hard work"
			40 benchFib.
			"update the job progress"
			job currentValue: i. ]
	] asJob.
	
	"run a different thread to interrupt the job"
	[ aJob run ] forkAt: Processor userBackgroundPriority.
	1 second asDelay wait. "wait for the job to start properly"
	aJob debug
	