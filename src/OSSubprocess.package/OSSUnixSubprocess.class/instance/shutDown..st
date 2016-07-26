system startup & shutdown
shutDown: quitting
	" IMPORTANT: read first the comment of OSSVMProcess >> shutDown:
	This method is called if this process is still running at the time a 'Quit image' is happening 
	in Pharo. If terminateOnShutdown is true, then we simply terminate the process via sigterm
	and collect it exit status.
	If it false, then we simply stop waiting for it, and allow to finish at OS level
	(althought it may become orphan process as the parent will be die).
	"
	quitting ifTrue: [ 
		terminateOnShutdown 
			ifTrue: [ 
				self terminate.
				"To avoid problems..let's wait until we have the	exit status"
				self waitForExitPolling.
			]
			ifFalse: [ self stopWaiting ]
	] 