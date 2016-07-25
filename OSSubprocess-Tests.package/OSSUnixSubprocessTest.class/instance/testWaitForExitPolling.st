tests - wait
testWaitForExitPolling
	| command | 
	
	command := self newCommand 
					command: '/bin/sleep';
					arguments: {'3'}.
	
	command run. 
	self assert: command pid > 0.
	self assert: [ command waitForExitPolling ] timeToRun asSeconds >= 2.
	self assert: command exitStatusInterpreter exitStatus equals: 0. 