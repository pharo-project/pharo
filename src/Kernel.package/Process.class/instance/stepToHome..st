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
	"error class = Halt ifTrue: [ 
		aContext methodNode 
			pragmaNamed: #debuggerCompleteToSender 
			ifPresent: [ ^ self completeTo: aContext sender ] ]."
	error ifNotNil: [^ suspendedContext := error signalerContext].
	^ suspendedContext