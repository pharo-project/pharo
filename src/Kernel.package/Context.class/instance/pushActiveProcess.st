instruction decoding
pushActiveProcess
	"Simulate the action of bytecode that pushes the the active Process on the 
	top of its own stack."

	self push: Processor activeProcess