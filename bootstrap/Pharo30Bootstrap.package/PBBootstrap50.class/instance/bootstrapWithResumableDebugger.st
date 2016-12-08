running
bootstrapWithResumableDebugger
	"If we do not fork the process, we get a post-mortem debugger"
	[ self bootstrap ]
		on: Error, Halt
		fork: [ :ex | 
				Smalltalk logError: ex description inContext: nil.
				ex pass ]