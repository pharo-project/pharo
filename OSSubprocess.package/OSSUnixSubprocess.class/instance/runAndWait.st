running
runAndWait
	"IMPORTANT: read first the comment of #waitForExit.
	
	This methods runs the subprocess AND waits until the child has exited. 
	"
	self run.
	self waitForExit.
	