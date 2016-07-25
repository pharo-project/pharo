tests - streams
testBasicCommandWriteToStdin
	| command stdOutString string streamsInfo | 
	streamsInfo := self getStreamsInfoForRunningTest.
	string := 'this is a test shat should have been printed in the stdout'.
	command := self newCommand
					command: '/bin/cat';
					redirectStdin;
					redirectStdout.
					
	command run.
	command stdinStream 
		nextPutAll: string;
		close.
	command waitForExit.
	stdOutString := command stdoutStream upToEndOfFile.
	command closeAndCleanStreams.
	self assert: command isSuccess.
	self assert: stdOutString equals: string.
	self assertStreamsInfoWithPrevious: streamsInfo.
	
	
