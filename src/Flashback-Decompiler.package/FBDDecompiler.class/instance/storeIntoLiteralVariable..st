data flow instructions
storeIntoLiteralVariable: association
	simulatedStack addLast: (builder codeAssignment: simulatedStack removeLast to: (builder codeVariable: association key))