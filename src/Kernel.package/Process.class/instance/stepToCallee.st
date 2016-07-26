changing suspended state
stepToCallee
	"Step until top context changes"

	Processor activeProcess
		evaluate:
			[| ctxt |
			ctxt := suspendedContext.
			[ctxt == suspendedContext] whileTrue: [
				suspendedContext := suspendedContext step]]
		onBehalfOf: self.
	^suspendedContext