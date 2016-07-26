changing suspended state
activateReturn: aContext value: value
	"Activate 'aContext return: value', so execution will return to aContext's sender"

	^Processor activeProcess
		evaluate: [suspendedContext := suspendedContext activateReturn: aContext value: value]
		onBehalfOf: self