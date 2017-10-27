changing suspended state
complete: aContext 
	"Run self until aContext is popped or an unhandled error is raised.  Return self's new top context, unless an unhandled error was raised then return the signaler context (rather than open a debugger)."
	
	| ctxt pair error |
	ctxt := suspendedContext.
	suspendedContext := nil.  "disable this process while running its stack in active process below"
	pair := Processor activeProcess
				evaluate: [ctxt runUntilErrorOrReturnFrom: aContext]
				onBehalfOf: self.
	suspendedContext := pair first.
	error := pair second.
	error ifNotNil: [
		suspendedContext := error signalerContext.
		suspendedContext push: error defaultResumeValue. "debugger will show signalling method and will be able continue execution. For this it needs some result of signalling"].
	^ suspendedContext