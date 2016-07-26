childs management
updateActiveChildren
	"Test each active child for its completion status and update exitStatus
	accordingly. This method may be called when a semaphore is set indicating that
	some child OSProcess has died. "

	self activeChildren do:
		[:child | child queryExitStatus ]
