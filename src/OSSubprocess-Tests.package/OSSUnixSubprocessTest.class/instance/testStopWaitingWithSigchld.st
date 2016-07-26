tests - wait
testStopWaitingWithSigchld
	| process counter oldZombies |
	process := self newCommand.
	oldZombies := self numberOfZombiesProcesses.
	counter := 0.
	[
	process
		command: 'tail';
		arguments:  (Array with: '-f' with: Smalltalk image imagePath);
		redirectStdout;
		runAndWaitPollingEvery: (Delay forMilliseconds: 50)  retrievingStreams: true onExitDo: [ :command :outString :errString |
			"The counter is to demonstrate that the exit happens only once we did the #stopWaiting
			and not before"
			self assert: counter equals: 0. 
			self assert: command exitStatus isNil.
			"Since we did not wait for the child to finish we must do a sigterm to avoid zombies
			when running the tests"
			command sigterm.
			"Not sure about while this delay but it seems it's needed so that the next queryExitStatus
			does collect exit status and hence avoid zombies"
			(Delay forSeconds: 4) wait.
			command queryExitStatus.
			self assert: self numberOfZombiesProcesses equals: oldZombies.
		]
	"The problem here is that we simply do a #fork this closure will continue to be executed while other test run. And so, all the test checking for zombies or opened files will fail because we can have this forked process that did not yet finish. A workaround is to simply run this closures with a higher priority so that there are less chances of other tests to be running. Another possibility would be to reject those 'tail' processes from #numberOfZombiesProcesses and #numberOfOpenFiles as all the tests that use #fork use the 'tail' OS command."	
	] forkAt: Processor userInterruptPriority.				

	(Delay forSeconds: 3) wait.
	"Just to demonstrate that tail will run forever until explicitly closed".
	self assert: process isRunning. 
	process stopWaiting.
	