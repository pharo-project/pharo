tests - shell
testShellCommandWithPipingStdout

	self newCommand
		shellCommand: 'ps -fea | grep Pharo';
		redirectStdout;
		runAndWaitOnExitDo: [ :command :outString |
			self assert: (outString includesSubstring: 'Pharo').						
		]
		 
