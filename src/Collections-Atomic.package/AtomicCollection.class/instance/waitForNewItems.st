signaling
waitForNewItems
	"Override in subclass. By default, yield current process.
	If you run more than one process, which popping items from queue,
	then there is no guarantee, that after wait, #next won't block the queue"
	^ self yield