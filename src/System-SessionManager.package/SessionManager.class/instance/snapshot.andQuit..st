snapshot and quit
snapshot: save andQuit: quit
	| isImageStarting |
	"We do the snapshot in a separate process in maximum priority to have always a clean startup.
	This process will be interrupted by the fork, and will be resumed as soon as the snapshot finishes."
	[ isImageStarting := self launchSnapshot: save andQuit: quit ]
		forkAt: Processor highestPriority.
	^ isImageStarting