data flow instructions
methodReturnTop
	^ currentSequence addNode: (builder codeReturn: simulatedStack removeLast)