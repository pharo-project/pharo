private
popFromStack: numElements
	^ ((1 to: numElements) collect: [ :i | simulatedStack removeLast ]) reverse