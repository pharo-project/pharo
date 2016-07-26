scripting
version41StartLogging
	"Start recording IDE events"

	#OmSessionStore asClass 
		defaultBaseLocator: FileLocator preferences / 'pharo' / 'epicea-3.3'.
	
	#EpMonitor asClass current enable.