scripting
version54StartLogging
	"Start recording IDE events if in interactive mode"

	Smalltalk isInteractive ifTrue: [
		#OmSessionStore asClass 
			defaultBaseLocator: FileLocator preferences / 'pharo' / 'epicea-5.4-sessions'.
		
		#EpMonitor asClass current enable ]