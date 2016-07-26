data flow instructions
doDup
	| dupTemp |
	simulatedStack last class == RBTemporaryNode
		ifTrue: [ ^ simulatedStack addLast: simulatedStack last ].
	self incrTempCount.
	dupTemp := self newTemp.
	currentSequence addNode: (builder codeAssignment: simulatedStack removeLast to: dupTemp).
	currentSequence addTemporaryNamed: 'tmp' , tempCount printString.
	simulatedStack 
		addLast: dupTemp;
		addLast: dupTemp.
		