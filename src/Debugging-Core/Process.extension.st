Extension { #name : 'Process' }

{ #category : '*Debugging-Core' }
Process >> stepToSendOrReturn [

	^Processor activeProcess
		evaluate: [suspendedContext := suspendedContext stepToSendOrReturn]
		onBehalfOf: self
]
