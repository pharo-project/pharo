tests - signals
testSigTermInsideLoop

	| process exited streamsInfo |
	
	exited := false.
	streamsInfo := self getStreamsInfoForRunningTest.
	process := self newCommand.
	
	process 
		command: 'tail';
		arguments: (Array with: '-f' with: Smalltalk image changesName);
		redirectStdout;
		redirectStderr;
		runAndWaitPollingEvery: (Delay forMilliseconds: 500) 
		doing: [ :aProcess :outStream :errStream |  
		 	"Imagine here that you have some condition...say.. elapsedTime > 1 h .. 
			or ... stdOutStream size > 1MB  or whatever..  "
			
			"#sigterm will throw an error (no such process) if the process with the given pid doesn't exist. Because of the looping of this method it COULD happen that we send #sigterm again even if the process was already killed in the previous loop. So we simply make sure the process is running before doing the #sigterm"
			aProcess isRunning ifTrue: [ aProcess sigterm. ]
 	]
		onExitDo: [ :aProcess :outStream :errStream  |
			process closeAndCleanStreams.
			exited := true.
			self assert: aProcess exitStatusInterpreter printString equals: 'exit due to signal 15'
		].

	self assert: exited.	
	self assertStreamsInfoWithPrevious: streamsInfo.
