scripting
version20StartLogging
	"Start recording IDE events"

	#EpSessionStoreStrategy asClass 
		defaultBaseDirectoryLocator: FileLocator preferences / 'pharo' / 'epicea-study-1'.
	
	#EpMonitor asClass current enable.