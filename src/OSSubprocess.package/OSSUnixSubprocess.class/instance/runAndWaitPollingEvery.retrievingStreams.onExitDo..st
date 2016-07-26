running
runAndWaitPollingEvery: aDelay retrievingStreams: retrieveStreams onExitDo: onExitClosure
	"IMPORTANT: first read the comment of #waitForExitPollingEvery:retrievingStreams:
	
	This method runs the subprocess, then waits for it doing an image-based delay polling. 
	If retrieveStreams is true, then as part of the polling loop, it also reads from the streams and
	appends the intemediate results in retrievedStdout / retrievedStderr. 
	Once the process has finished we evaluate onExitClosure.
	If retrieveStreams was true, before evaluating onExitClosure, we automatically close streams
	so that there is nothing else to be done for the user, and we also pass as arguments the
	retrievedStdout / retrievedStderr  to the closure.
	
	With this method, the user does not need to do anything else. 
	 
	"
	self run.
	self waitForExitPollingEvery: aDelay retrievingStreams: retrieveStreams.
	retrieveStreams 
		ifTrue: [ 
			self closeAndCleanStreams.
			onExitClosure cull: self cull: retrievedStdout contents cull: retrievedStderr contents.
		]
		ifFalse: [ 
			onExitClosure cull: self.
		]
	
	