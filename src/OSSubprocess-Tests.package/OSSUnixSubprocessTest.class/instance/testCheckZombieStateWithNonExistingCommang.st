tests - child status
testCheckZombieStateWithNonExistingCommang
	| command oldZombies | 
	oldZombies := self numberOfZombiesProcesses.
	command := self newCommand 
				command: 'whatever'.
				
		"Ideally, I would like to use a should:raise: but not all OS do fail. OSX does send errorl, while Linux does not.
	So...if error, then it;s ok. But if not, also."
	[ command runAndWait ] on: Error do: [  :ex | 
		self assert: (ex printString includesSubstring: 'No such file or directory') .
		self assert: command pid isNil.
		^ nil
	].
	
	self assert: command exitStatusInterpreter exitStatus > 0.
	self assert: self numberOfZombiesProcesses equals: oldZombies.
	
	