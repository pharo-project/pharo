instruction decoding
popIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex
	"Remove Top Of Stack And Store Into Offset of Temp Vector bytecode."
	stackPointer := stackPointer - 1