scripting
version59StartLogging
	"Start recording IDE events if in interactive mode"

	Smalltalk isInteractive ifTrue: [
		(Smalltalk classNamed: #OmSessionStore)
			defaultBaseLocator: FileLocator preferences / 'pharo' / 'epicea-sessions'.
		
		(Smalltalk classNamed: #EpMonitor) current enable ]