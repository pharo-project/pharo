private
resumeProcess
	"Make sure the interrupted process is restored properly and restart the low space handler"
	
	interruptedProcess isTerminated
		ifFalse: [errorWasInUIProcess
				ifTrue: [UIManager default resumeUIProcess: interruptedProcess]
				ifFalse: [interruptedProcess resume]].

	Smalltalk installLowSpaceWatcher.	"restart low space handler".
		
	