instructions
pushClosureCopyCopiedValues: copiedValuesNames args: args jumpTo: aJumpLabel
	
	|  irInstruction |
	
	irInstruction := IRInstruction pushClosureCopyCopiedValues: copiedValuesNames args: args.
	self add: irInstruction.
	(jumpAheadStacks at: aJumpLabel ifAbsentPut: [OrderedCollection new]) addLast: irInstruction.
	self startNewSequence.
	irInstruction blockSequence: currentSequence.
	self pushScope: irInstruction.
	self addTemps: args.
	self addTemps: copiedValuesNames.


	^irInstruction