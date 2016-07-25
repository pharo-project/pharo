running
runAndWaitOnExitDo: anExitClosure
	"This is a high end API for #runAndWait. IMPORTANT: read the comment in #runAndWait to know when to use this API.
	The facility this method adds over #runAndWait is that when the process has finished it:
	1) Retrieves all contents from stdout and stderr (if they were defined)
	2) It automatically closes the streams mapped to stdout/stderr (if defined)
	3) It allows you to plug anExitClosure which will be evaluated with this subprocess and the contents of stdout and stderr. 
	
	With this method, the user does not need to do anything else. 
	"
	self run.
	self waitForExit.
	self retrieveWhatIsAvailableOn: stdoutStream andAddItTo: retrievedStdout.
	self retrieveWhatIsAvailableOn: stderrStream andAddItTo: retrievedStderr.
	self closeAndCleanStreams.
	anExitClosure cull: self cull: retrievedStdout contents cull: retrievedStderr contents.
	