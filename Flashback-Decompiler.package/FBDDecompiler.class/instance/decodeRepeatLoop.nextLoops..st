control flow instructions
decodeRepeatLoop: loop nextLoops: loops
	| pastSequence  |
	
	pastSequence := currentSequence.
	
	currentSequence := builder codeEmptySequence.
	self interpret: loop backjump - 1 nextLoops: loops.
	instructionStream nextInstruction. "skip backjump"
	
	simulatedStack addLast:  (builder codeRepeatNode:  currentSequence).
	currentSequence := pastSequence.
	self doPop.