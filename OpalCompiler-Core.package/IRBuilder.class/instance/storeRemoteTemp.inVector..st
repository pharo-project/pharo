instructions
storeRemoteTemp: name inVector: nameOfVector

	^self add: (IRInstruction storeRemoteTemp: name inVectorAt: nameOfVector)