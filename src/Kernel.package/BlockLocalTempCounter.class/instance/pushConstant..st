instruction decoding
pushConstant: value
	"Push Constant, value, on Top Of Stack bytecode."
	stackPointer := stackPointer + 1