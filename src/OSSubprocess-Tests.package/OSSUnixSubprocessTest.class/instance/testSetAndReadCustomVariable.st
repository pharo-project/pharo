tests - env
testSetAndReadCustomVariable
	self newCommand 
		shellCommand: 'echo ${WHATEVER}';
		environmentAt: 'WHATEVER' put: 'hello';
		redirectStdout;
		runAndWaitOnExitDo: [ :command :outString |
			self assert: outString trimmed equals: 'hello'
		]
