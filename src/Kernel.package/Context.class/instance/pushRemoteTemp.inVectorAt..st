instruction decoding
pushRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex
	"Simulate the action of bytecode that pushes the value at remoteTempIndex
	 in one of my local variables being used as a remote temp vector."
	self push: ((self at: tempVectorIndex + 1) at: remoteTempIndex + 1)