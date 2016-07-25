data flow instructions
storeIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex
	simulatedStack addLast: (builder codeAssignment: simulatedStack removeLast to: ((simulatedStack at: tempVectorIndex + 1) at: remoteTempIndex + 1))