tests - child status
testCheckZombieStateWithWaitChild
	| command oldZombies | 
	oldZombies := self numberOfZombiesProcesses.
	command := self newCommand 
				command: '/bin/sleep';
				arguments: {'1'}.
	
	command run.
	command waitForExit. 
	self assert: self numberOfZombiesProcesses equals: oldZombies.
	
	