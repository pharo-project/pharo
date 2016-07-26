instruction decoding
pushNewArrayOfSize: numElements 
	"Push New Array of size numElements bytecode."
	stackPointer := stackPointer + 1