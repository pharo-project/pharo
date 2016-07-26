instruction decoding
pushRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex
	"Push Contents at Offset in Temp Vector bytecode."
	stackPointer := stackPointer + 1