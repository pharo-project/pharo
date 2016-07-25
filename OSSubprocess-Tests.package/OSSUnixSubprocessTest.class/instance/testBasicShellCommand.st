tests - shell
testBasicShellCommand
	| command | 
	
	command := self newCommand
					shellCommand: 'ls'.
	
	command runAndWait.
	