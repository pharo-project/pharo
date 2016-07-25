childs management
unregisterChildProcess: anOSProcess

	"Unregister anOSProcess from the list of children"

	accessProtect critical: [childProcessList remove: anOSProcess ifAbsent: [] ].
	^ anOSProcess
