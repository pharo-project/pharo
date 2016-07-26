changing suspended state
step

	^Processor activeProcess
		evaluate: [suspendedContext := suspendedContext step]
		onBehalfOf: self