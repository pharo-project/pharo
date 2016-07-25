tests - streams
testReadingFromStdoutAfterCommandFinishesDoesNotBlocksVM
	| command streamsInfo | 
	streamsInfo := self getStreamsInfoForRunningTest.
	command := self newCommand
					command: '/bin/sleep';
					arguments: (Array with: '15');
					redirectStdout;
					runAndWait.
					
	"Even with a blocking stream, if the command has finished, we have
	all data ready to read and hence it's not blocking"
	self assert:[ command stdoutStream upToEndOfFile ] timeToRun seconds <= 5. 
	command closeAndCleanStreams.
	self assertStreamsInfoWithPrevious: streamsInfo.
	

	