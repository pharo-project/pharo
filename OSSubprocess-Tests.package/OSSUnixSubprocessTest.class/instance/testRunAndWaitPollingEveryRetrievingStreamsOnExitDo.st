tests - wait
testRunAndWaitPollingEveryRetrievingStreamsOnExitDo
	self newCommand 
		command: '/bin/ls';
		arguments: (Array with: Smalltalk image imagePath);
		redirectStdout;
		runAndWaitPollingEvery: (Delay forMilliseconds: 50)  retrievingStreams: true onExitDo: [ :command :outString :errString |
			self assert: command pid > 0.
			self assert: command exitStatusInterpreter exitStatus equals: 0. 
			self assert: (outString includesSubstring: Smalltalk image imagePath).
			self assert: command stdoutStream closed.
		]				
