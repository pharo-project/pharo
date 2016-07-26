tests - streams
testReadingFromStdoutDoesNotBlocksVM
	| command customStream | 
	customStream := self systemAccessor makeNonBlockingPipe.
	command := self newCommand
					command: '/bin/sleep';
					arguments: (Array with: '4');
					redirectStdoutTo: customStream.
	
	command run. 
	self assert:[ command stdoutStream upToEnd ] timeToRun seconds <= 2. 
	command waitForExit. 
	command closeAndCleanStreams.