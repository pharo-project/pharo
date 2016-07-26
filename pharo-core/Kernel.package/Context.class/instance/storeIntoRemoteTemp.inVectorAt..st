instruction decoding
storeIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex
	"Simulate the action of bytecode that stores the top of the stack at
	 an offset in one of my local variables being used as a remote temp vector."

	(self at: tempVectorIndex + 1) at: remoteTempIndex + 1 put: self top