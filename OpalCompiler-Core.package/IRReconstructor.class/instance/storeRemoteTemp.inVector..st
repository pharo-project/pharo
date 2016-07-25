instructions
storeRemoteTemp: name inVector: nameOfVector
	| anInstruction |
	anInstruction := super storeRemoteTemp: name inVector: nameOfVector.
	self rememberReference: anInstruction to: nameOfVector in: remoteTemps.