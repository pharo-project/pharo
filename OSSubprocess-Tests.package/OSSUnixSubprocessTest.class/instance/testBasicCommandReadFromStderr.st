tests - streams
testBasicCommandReadFromStderr
	| streamsInfo |
	streamsInfo := self getStreamsInfoForRunningTest.
	self newCommand
		command: '/bin/ls';
		arguments: (Array with: '-' with: 'foo');
		redirectStderr;
		runAndWaitOnExitDo: [ :command :outString :errString |
			self assert: (errString includesSubstring: 'foo: No such file or directory').
			self assert: command stdoutStream isNil.
			self assert: command stderrStream closed.
			self assertStreamsInfoWithPrevious: streamsInfo.
		].
	
	