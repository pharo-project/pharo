instruction decoding
popIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex
	"Simulate the action of bytecode that removes the top of the stack and  stores
	 it into an offset in one of my local variables being used as a remote temp vector."

	(self at: tempVectorIndex + 1) at: remoteTempIndex + 1 put: self pop