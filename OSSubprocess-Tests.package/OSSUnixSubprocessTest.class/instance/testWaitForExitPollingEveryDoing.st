tests - high API
testWaitForExitPollingEveryDoing
	| streamsInfo totalRead |
	totalRead := String new writeStream.
	streamsInfo := self getStreamsInfoForRunningTest.
	self newCommand
		command: '/bin/ls';
		arguments: (Array with: Smalltalk image imagePath);
		redirectStdout;
		redirectStderr;
		runAndWaitPollingEvery: (Delay forMilliseconds: 50) 
		doing: [ :process :outStream :errStream |  
			| read | 
			read := outStream upToEnd.
			totalRead nextPutAll: read.
		]
		onExitDo: [ :process :outStream :errStream  |
			self assert: (totalRead contents includesSubstring: Smalltalk image imagePath).
			process closeAndCleanStreams.
			self assertStreamsInfoWithPrevious: streamsInfo.
		].

	