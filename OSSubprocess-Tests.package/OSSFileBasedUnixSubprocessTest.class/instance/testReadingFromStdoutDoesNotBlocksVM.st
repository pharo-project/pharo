tests
testReadingFromStdoutDoesNotBlocksVM
	| command | 
	"With files, the reading from stdout does not lock the VM"
	command := self newCommand
					command: '/bin/sleep';
					arguments: (Array with: '4');
					redirectStdout.
	
	command run. 
	self assert:[ command stdoutStream upToEnd ] timeToRun seconds <= 1. 
	command waitForExit.
	command closeAndCleanStreams.