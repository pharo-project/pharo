tests - child status
testGetChildPid
	| command | 
	
	command := self newCommand 
					command: '/bin/ls'.
	
	command run.
	self assert: command pid > 0.
	command waitForExit. 