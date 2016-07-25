memory space
installLowSpaceWatcher
	"Start a process to watch for low-space conditions."

	"Smalltalk installLowSpaceWatcher"

	self primSignalAtBytesLeft: 0.	"disable low-space interrupts"
	LowSpaceProcess ifNotNil: [ LowSpaceProcess terminate ].
	LowSpaceProcess := [ self lowSpaceWatcher ] newProcess.
	LowSpaceProcess name: 'Low Space Watcher'.
	LowSpaceProcess priority: Processor lowIOPriority.
	LowSpaceProcess resume