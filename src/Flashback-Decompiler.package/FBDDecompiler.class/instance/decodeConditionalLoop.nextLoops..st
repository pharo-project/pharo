control flow instructions
decodeConditionalLoop: loop nextLoops: loops
	| condSeq pastSequence boolean |
	
	pastSequence := currentSequence.
	
	condSeq := builder codeEmptySequence.
	currentSequence := condSeq.
	self interpret: loop exitCondition - 1 nextLoops: loops.
	self doPop.
	boolean := instructionStream willJumpIfFalse.
	instructionStream nextInstruction. "skip the conditional jump exiting the loop"
	
	currentSequence := builder codeEmptySequence.
	self interpret: loop backjump - 1.
	instructionStream nextInstruction. "skip the backjump"
	
	simulatedStack addLast:  (builder 
		codeLoopNode: boolean
		condition: condSeq
		body: currentSequence).
	currentSequence := pastSequence.
	self doPop.