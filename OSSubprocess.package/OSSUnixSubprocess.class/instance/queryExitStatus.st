status
queryExitStatus
	"This method does the waitpid() to check the exit status of the process. 
	It's important to ALWAYS send this message after the process has exited
	because if we do not collect the exit status, we will leave zombies. 
	This method could be called directly from the childWatcher process of the OSSVMProcess 
	when a SIGCHLD was received (#updateActiveChildrenAndNotifyDead) or from a image-side polling
	 like #waitForExitPollingEvery:doing:"
	^ exitStatus ifNil: [ 
			exitStatus := self systemAccessor waitpidNoHang: pid.
			exitStatus
		]
		