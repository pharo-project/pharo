data flow instructions
pushReceiverVariable: offset 
	simulatedStack addLast: (builder codeInstanceVariable: offset + 1)
