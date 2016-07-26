waiting
waitForExitPollingEvery: aDelay retrievingStreams: retrieveStreams
	" IMPORTANT: read first the comment of #waitForExit. 
	Second, read the comment of #waitForExitPollingEvery:doing:
	
	If retrieveStreams is true, we read from stdout/stderr as part of the loop and
	append the answers in retrievedStdout / retrievedStderr.
	And this solves the deadlock mentioned in #waitForExit.
	
	This method (with retrieveStreams in true) should be used when mapping stdout / stderr with pipes
	and the command to be executed may write a lot to it. Does not make much sense to be used with regular files. 
	
	Answers the exit status of the child."
	^ self 
		waitForExitPollingEvery: aDelay
		doing: [ 
			retrieveStreams ifTrue: [ 
				self retrieveWhatIsAvailableOn: stdoutStream andAddItTo: retrievedStdout.
				self retrieveWhatIsAvailableOn: stderrStream andAddItTo: retrievedStderr.
			]
		] 
	