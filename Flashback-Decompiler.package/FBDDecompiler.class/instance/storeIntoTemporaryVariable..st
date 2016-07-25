data flow instructions
storeIntoTemporaryVariable: offset
	| val |
	simulatedStack addLast: (builder codeAssignment: (val := simulatedStack removeLast) to: (simulatedStack at: offset +1)).	
	(loopsArray at: self pc) ifNotNil: [ self doPop. simulatedStack addLast: val ]