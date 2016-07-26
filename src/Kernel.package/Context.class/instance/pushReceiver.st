instruction decoding
pushReceiver
	"Simulate the action of bytecode that pushes the active context's receiver 
	on the top of the stack."

	self push: self receiver