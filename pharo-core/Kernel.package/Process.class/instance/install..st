changing suspended state
install: aContext 
	"Replace the suspendedContext with aContext."

	self == Processor activeProcess
		ifTrue: [^self error: 'The active process cannot install contexts'].
	suspendedContext := aContext