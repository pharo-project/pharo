sampling
testSampling1

	| tally |
	tally := MessageTally
				spyOn: [ 1000  timesRepeat: [ 3.14159 printString  ] ]  
				reportOtherProcesses: false 
				cutoff: 1 
				openResultWindow: false
				closeAfter: false.

	"No process is accessible from the tally, the reason is that using the sampling method execute the block in the current process"
	self assert: (tally process isNil).

