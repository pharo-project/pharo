tests - streams
testBasicCommandWriteToStdin2
	| command streamsInfo outString | 
	streamsInfo := self getStreamsInfoForRunningTest.

	command := self newCommand
					command: 'wc';
					redirectStdin;
					redirectStdout;
					run. 

	command stdinStream 
			nextPutAll: Smalltalk image imagePath;
			close.

	command waitForExit.
	outString := command stdoutStream upToEndOfFile. 
	command closeAndCleanStreams.				
	self assert: command isSuccess.
	"It's hard to test the output of wc crossplatform..."
	self assert: outString size > 5.
	self assertStreamsInfoWithPrevious: streamsInfo.
	
	
