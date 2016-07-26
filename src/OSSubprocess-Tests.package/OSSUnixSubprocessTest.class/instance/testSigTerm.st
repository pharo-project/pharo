tests - signals
testSigTerm

	| process exited streamsInfo |
	
	exited := false.
	streamsInfo := self getStreamsInfoForRunningTest.
	process := self newCommand.
	[
	process 
		command: 'tail';
		arguments: (Array with: '-f' with: Smalltalk image changesName);
		redirectStdout;
		redirectStderr;
		runAndWaitPollingEvery: (Delay forMilliseconds: 500) 
		doing: [ :aProcess :outStream :errStream |  
		 	outStream upToEnd.
			errStream upToEnd.
		]
		onExitDo: [ :aProcess :outStream :errStream  |
			process closeAndCleanStreams.
			exited := true.
			self assert: aProcess exitStatusInterpreter printString equals: 'exit due to signal 15'
		].
	"The problem here is that we simply do a #fork this closure will continue to be executed while other test run. And so, all the test checking for zombies or opened files will fail because we can have this forked process that did not yet finish. A workaround is to simply run this closures with a higher priority so that there are less chances of other tests to be running. Another possibility would be to reject those 'tail' processes from #numberOfZombiesProcesses and #numberOfOpenFiles as all the tests that use #fork use the 'tail' OS command."	
	] forkAt: Processor userInterruptPriority.

	(Delay forSeconds: 2) wait. 
	process sigterm. 
	(Delay forSeconds: 1) wait. 
	self assert: exited.	
	self assertStreamsInfoWithPrevious: streamsInfo.
