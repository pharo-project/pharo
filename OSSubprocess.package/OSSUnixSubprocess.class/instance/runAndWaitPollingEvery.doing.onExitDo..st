running
runAndWaitPollingEvery: aDelay doing: aClosure onExitDo: onExitClosure
	"IMPORTANT: first read the comment of #waitForExitPollingEvery:doing:
	
	This method runs the subprocess, then waits for it doing an image-based delay polling. 
	It does not automatically retrieve from streams nor closes them on exit. 
	The user must explicitly read from streams in aClosure otherwise it might 
	have the problem mentioned in the comment of #waitForExit.
	The user must also close streams at some point. Either inside the #onExitClosure
	or later, but should be done. 
		 
	"
	self run.
	self waitForExitPollingEvery: aDelay doing: aClosure.
	onExitClosure cull: self cull: stdoutStream cull: stderrStream.
	