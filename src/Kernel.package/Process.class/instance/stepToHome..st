changing suspended state
stepToHome: aContext 
	| ctxt pair error |
	ctxt := suspendedContext.
	suspendedContext := nil.
	pair := Processor activeProcess
				evaluate: [ctxt stepToHome: aContext]
				onBehalfOf: self.
	suspendedContext := pair first.
	error := pair second.
	error ifNotNil: [
		suspendedContext := error signalerContext.
		suspendedContext push: error defaultResumeValue. "debugger will show signalling method and will be able continue execution. For this it needs some result of signalling"].
	^ suspendedContext