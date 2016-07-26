data flow instructions
storeIntoReceiverVariable: offset 
	simulatedStack addLast: (builder codeAssignment: simulatedStack removeLast to: (builder codeInstanceVariable: offset + 1)).
