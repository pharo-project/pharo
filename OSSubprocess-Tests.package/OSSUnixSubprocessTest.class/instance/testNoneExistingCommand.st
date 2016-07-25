tests - basic
testNoneExistingCommand
	| command stdout stderr | 
	
	command := self newCommand
					command: 'whatever';
					redirectStdout;
					redirectStderr.
	
	"Ideally, I would like to use a should:raise: but not all OS do fail. OSX does send errorl, while Linux does not.
	So...if error, then it;s ok. But if not, also."
	[ command runAndWait ] on: Error do: [  :ex | 
		self assert: (ex printString includesSubstring: 'No such file or directory') .
		self assert: command pid isNil.
		command closeAndCleanStreams.
		^ nil
	].
	
	self assert: command exitStatusInterpreter exitStatus > 0.
	command closeAndCleanStreams.