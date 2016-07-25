data flow instructions
popIntoTemporaryVariable: offset
	simulatedStack last class = Array
		ifTrue: [ 
			"Temp vector"
			currentSequence removeTemporaryNamed: (simulatedStack at: offset + 1) name.
			simulatedStack at: offset + 1 put: simulatedStack removeLast ]
		ifFalse: [ 
			simulatedStack
				addLast: (builder codeAssignment: simulatedStack removeLast to: (simulatedStack at: offset + 1)). self doPop ]	