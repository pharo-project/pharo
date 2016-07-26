tests - child status
testGetChildStatusBeforeAndAfterFinishing
	| command oldZombies | 
	oldZombies := self numberOfZombiesProcesses.
	command := self newCommand 
				command: '/bin/sleep';
				arguments: {'3'}.
	
	command run. 
	self assert: command pid > 0.
	(Duration seconds: 1) wait.
	self assert: command queryExitStatus isNil.
	self deny: command isComplete.
	self assert: command isRunning.
	command waitForExit.
	self assert: command queryExitStatus isNil not.
	self assert: command exitStatusInterpreter exitStatus equals: 0.
	self assert: command isComplete.
	self assert: self numberOfZombiesProcesses equals: oldZombies.
	