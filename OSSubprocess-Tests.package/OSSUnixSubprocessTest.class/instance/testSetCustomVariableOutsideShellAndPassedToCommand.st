tests - env
testSetCustomVariableOutsideShellAndPassedToCommand
	" This test demonstrates that we do not expand variables automatically. So if you don't use the shell, or any other special command that
	will search for special variables (like git searching $GIT_EDITOR), these will not be resolved. 
	If you want that, then you must execute the command with the arguments already expandaded. That is, for example,
	in this case, you should pass the argument 'hello' rather than ${WHATEVER} "
	
	self newCommand 
		command: '/bin/echo';
		arguments: (Array with: '${WHATEVER}');
		environmentAt: 'WHATEVER' put: 'hello';
		redirectStdout;
		runAndWaitOnExitDo: [ :command :outString |
			self assert: outString trimmed equals: '${WHATEVER}'. 			
		]
