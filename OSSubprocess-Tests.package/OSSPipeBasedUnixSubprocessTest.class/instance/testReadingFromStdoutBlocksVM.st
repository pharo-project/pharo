tests - streams
testReadingFromStdoutBlocksVM
	| command customStream | 
	customStream := self systemAccessor makeBlockingPipe.
	command := self newCommand
					command: '/bin/sleep';
					arguments: (Array with: '4');
					redirectStdoutTo: customStream.
	
	command run. 
	self assert:[ command stdoutStream upToEnd ] timeToRun seconds > 2. 
	command waitForExit.
	command closeAndCleanStreams.