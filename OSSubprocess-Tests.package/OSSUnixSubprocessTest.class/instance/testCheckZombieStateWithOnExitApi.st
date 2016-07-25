tests - child status
testCheckZombieStateWithOnExitApi
	| command oldZombies | 
	oldZombies := self numberOfZombiesProcesses.
	command := self newCommand 
				command: '/bin/sleep';
				arguments: {'1'};
				runAndWaitOnExitDo: [  ].
	
	self assert: self numberOfZombiesProcesses equals: oldZombies.
	
	