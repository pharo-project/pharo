system simulation
completeCallee: aContext
	"Simulate the execution of bytecodes until a return to the receiver."
	| context current nextContext |
	context := aContext.
	[ context == current or: [ context hasSender: self ] ]
		whileTrue: [
			current := context.
			nextContext := context quickStep.
			nextContext ifNil: [ self halt ].
			context := nextContext ].
	^ self stepToSendOrReturn