background process
startUp
	"Install a background process of the lowest possible priority that is always runnable."
	"Details: The virtual machine requires that there is always some runnable process that can be scheduled; this background process ensures that this is the case."

	Smalltalk installLowSpaceWatcher.
	BackgroundProcess ifNotNil: [BackgroundProcess terminate].
	BackgroundProcess := [self idleProcess] newProcess.
	BackgroundProcess priority: SystemRockBottomPriority.
	BackgroundProcess name: 'Idle Process'.
	BackgroundProcess resume.
