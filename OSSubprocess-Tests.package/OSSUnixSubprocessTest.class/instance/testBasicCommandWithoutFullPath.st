tests - basic
testBasicCommandWithoutFullPath

	self newCommand
		command: 'ls';
		arguments: (Array with: Smalltalk image imagePath);
		redirectStdout;
		runAndWaitOnExitDo: [ :command :outString |
			self assert: (outString includesSubstring: Smalltalk image imagePath).					
		]
		 
