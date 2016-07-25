tests - streams
testCreateMissingStandardStreamsEnabled
	| command streamsInfo | 
	streamsInfo := self getStreamsInfoForRunningTest.
	"The output of the ls command should be displayed in the stdout of the Pharo image. See comment of #createPipesForMissingStreams: for more details"
	command := self newCommand
					command: '/bin/ls';
					createMissingStandardStreams: true.
					
	command runAndWait. 
	self assert: command stdinStream isNil not.
	self assert: command stdoutStream isNil not.
	self assert: command stderrStream isNil not.
	command closeAndCleanStreams.
	self assertStreamsInfoWithPrevious: streamsInfo.
	