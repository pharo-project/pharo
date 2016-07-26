control flow instructions
interpretSeq: seq baseStackSize: stackSize distance: distance
	currentSequence := seq.
	self interpret: self pc + distance - 1.
	^ stackSize = simulatedStack size 
		ifFalse: [ self doPop. true ]
		ifTrue: [ false ]