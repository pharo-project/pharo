changing suspended state
stepToSendOrReturn

	^Processor activeProcess
		evaluate: [suspendedContext := suspendedContext stepToSendOrReturn]
		onBehalfOf: self