instructions
pushRemoteTemp: name inVector: nameOfVector

	^self add: (IRInstruction pushRemoteTemp: name inVectorAt: nameOfVector)