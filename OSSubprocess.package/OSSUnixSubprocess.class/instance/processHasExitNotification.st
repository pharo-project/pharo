status
processHasExitNotification
	"This method is called from the OSSVMProcess when its child watcher detects that this process has
	exited (via the receive of SIGCHLD) and so it notifies to it (via #updateActiveChildrenAndNotifyDead).
	We signal mutexForSigchld so that the #runAndWait can now finish.
	Note that the mutexForSigchld could be nil if the waitForExit API was not used"
	mutexForSigchld ifNotNil: [ mutexForSigchld signal ]