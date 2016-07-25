system simulation
stepToSendOrReturn
	"Simulate the execution of bytecodes until either sending a message or 
	returning a value to the receiver (that is, until switching contexts)."

	| context |
	[ self willSend or: [  self willReturn or: [ self willStore or: [self willCreateBlock ] ] ] ]
		whileFalse: [
			context := self step.
			context == self ifFalse: [
				"Caused by mustBeBoolean handling"
				^context ]]