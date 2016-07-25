changing suspended state
step: aContext 
	"Resume self until aContext is on top, or if already on top, do next step"

	^Processor activeProcess
		evaluate:
			[self suspendedContext == aContext
				ifTrue: [suspendedContext := suspendedContext step]
				ifFalse: [self complete: (self calleeOf: aContext)]]
		onBehalfOf: self