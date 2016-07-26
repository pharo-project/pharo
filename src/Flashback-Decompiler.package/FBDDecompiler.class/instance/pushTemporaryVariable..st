data flow instructions
pushTemporaryVariable: offset
	simulatedStack addLast: (simulatedStack at: offset + 1)