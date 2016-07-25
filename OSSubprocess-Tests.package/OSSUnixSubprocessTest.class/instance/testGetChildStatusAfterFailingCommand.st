tests - child status
testGetChildStatusAfterFailingCommand
	| command oldZombies | 
	oldZombies := self numberOfZombiesProcesses.
	command := self newCommand
					command: '/bin/ls';
					arguments: {'whatever'}.
	command run.
	self assert: command pid > 0.
	command waitForExit.
	command queryExitStatus.
	self assert: command exitStatusInterpreter exitStatus >  0. 
	self deny: command isSuccess.
	self assert: command isComplete.
	self deny: command isRunning.
	self assert: self numberOfZombiesProcesses equals: oldZombies.