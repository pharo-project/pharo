tests - env
testSetAndReadCustomVariableOutsideShell
	self newCommand 
		command: 'printenv';
		arguments: (Array with: 'WHATEVER');
		environmentAt: 'WHATEVER' put: 'hello';
		redirectStdout;
		runAndWaitOnExitDo: [ :command :outString |
			self assert: outString trimmed equals: 'hello'. 					
		]
