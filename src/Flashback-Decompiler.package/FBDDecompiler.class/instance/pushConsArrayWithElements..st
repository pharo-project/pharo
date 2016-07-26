data flow instructions
pushConsArrayWithElements: numElements
	| array |
	array := Array new: numElements.
	numElements to: 1 by: -1 do: [ :i | array at: i put: simulatedStack removeLast ].
	simulatedStack addLast: (builder codeArray: array)