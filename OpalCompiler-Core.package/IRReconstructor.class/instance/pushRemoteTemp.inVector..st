instructions
pushRemoteTemp: name inVector: nameOfVector
	| anInstruction |
	anInstruction := super pushRemoteTemp: name inVector: nameOfVector.
	self rememberReference: anInstruction to: nameOfVector in: remoteTemps.