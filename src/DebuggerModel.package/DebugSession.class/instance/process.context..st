initialization
process: aProcess context: aContext

	"aProcess stepToSendOrReturn ."
	"aProcess isSuspended ifTrue: [ aProcess stepToSendOrReturn ]."
	interruptedProcess := aProcess.
	interruptedContext := self filterTopContext: aContext.
	interruptedContext ifNil: [interruptedContext := aContext].
	
