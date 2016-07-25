tests - shell
testCustomShellWithStdout

	self newCommand
		shell: '/bin/bash' command: 'ls ', Smalltalk image imagePath;
		redirectStdout;
		runAndWaitOnExitDo: [ :command :outString |
			self assert: (outString includesSubstring: Smalltalk image imagePath).					
		]
		 
