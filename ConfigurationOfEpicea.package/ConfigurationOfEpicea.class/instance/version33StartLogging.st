scripting
version33StartLogging
	"Start recording IDE events"

	#OmSessionStore asClass 
		defaultBaseDirectoryLocator: FileLocator preferences / 'pharo' / 'epicea-3.3'.
	
	#EpMonitor asClass current enable.