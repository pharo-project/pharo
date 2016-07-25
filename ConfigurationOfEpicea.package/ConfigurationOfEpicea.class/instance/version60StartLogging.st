scripting
version60StartLogging
	"Start recording IDE events"

	(Smalltalk classNamed: #OmSessionStore)
		defaultBaseLocator: FileLocator imageDirectory / 'epicea-sessions'.
		
	(Smalltalk classNamed: #EpMonitor) current enable