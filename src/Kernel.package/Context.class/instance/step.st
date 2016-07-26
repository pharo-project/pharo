system simulation
step
	"Simulate the execution of the receiver's next bytecode. Answer the 
	context that would be the active context after this bytecode."

	^self interpretNextInstructionFor: self