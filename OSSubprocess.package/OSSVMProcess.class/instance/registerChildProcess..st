childs management
registerChildProcess: anOSProcess
	"Unregister anOSProcess, and trim the child process registry to prevent excessive
	accumulation of exited children."

	accessProtect critical: [ childProcessList addFirst: anOSProcess ].
	self pruneExitedChildrenAfter: childListTreshold.
	^ anOSProcess
	