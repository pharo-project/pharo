system simulation
stepUntilSomethingOnStack
	"Simulate the execution of bytecodes until either sending a message or 
	returning a value to the receiver (that is, until switching contexts)."

	| context |
	[ stackp = 0 ]
		whileTrue: [
			self isDead ifTrue: [ ^ self ].
			context := self step.
			context == self ifFalse: [
				"Caused by mustBeBoolean handling"
				^context ]]