system startup & shutdown
terminateOnShutdown
	" IMPORTANT: read first the comment of OSSVMProcess >> shutDown:
	If terminateOnShutdown is true, then the external process will be terminated if it was still
	running at the time a system quit image is happening.
	If false, then we will simply stop waiting for it and allow it to finish (thought it will likely become a orphan).
	"
	terminateOnShutdown := true