tests - wait
testWaitpidIsNoneBocking
	| command childStatus | 
	
	command := self newCommand
					command: '/bin/sleep';
					arguments: {'5'}.
	
	command run. 
	self assert: command pid > 0.
	(Duration seconds: 1) wait.
	self assert: [ childStatus := command exitStatusInterpreter ] timeToRun asSeconds < 1.
	self assert: childStatus isNil.
	command waitForExit.