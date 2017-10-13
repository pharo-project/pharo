snapshot and quit
snapshot: save andQuit: quit
	| isImageStarting wait |
	"We do the snapshot in a separate process in maximum priority to have always a clean startup.
	This process will be interrupted by the fork, and will be resumed as soon as the snapshot finishes.
	We synchronize these processes in case both are in the same priority"
	wait := Semaphore new.
	[
		isImageStarting := self launchSnapshot: save andQuit: quit.
		wait signal
	] forkAt: Processor highestPriority.
	wait wait.
	^ isImageStarting