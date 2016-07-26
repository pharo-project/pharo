tests - streams
testCustomStdoutStream
	| customStream streamsInfo | 
	streamsInfo := self getStreamsInfoForRunningTest.
	customStream := self newCommand createADefaultWriteStream.
	self newCommand
		command: '/bin/ls';
		arguments: (Array with: Smalltalk image imagePath);
		redirectStdoutTo: customStream;
		runAndWaitOnExitDo: [ :command :outString |
			self assert: (outString includesSubstring: Smalltalk image imagePath).
			self assert: command stdoutStream == customStream	.				
			self assertStreamsInfoWithPrevious: streamsInfo.
		].
	
	

