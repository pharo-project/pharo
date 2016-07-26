tests - child status
testCheckZombieStateAndTestChildWatcherWorking
	| command oldZombies | 
	oldZombies := self numberOfZombiesProcesses.
	command := self newCommand 
				command: '/bin/sleep';
				arguments: {'1'}.
	
	command run. 
	self assert: command pid > 0.
	(Duration seconds: 2) wait.
	"In this case we are NOT going to do the queryExitStatus since we want to know 
	if the child watcher is doing it's job."
	"self assert: command queryExitStatus isNil not."
	"Now it should not be a zombie anymore"
	self assert: self numberOfZombiesProcesses equals: oldZombies.
	
	