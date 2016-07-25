instructions
jumpAheadTo: labelSymbol if: boolean
	"Conditional jump to the sequence that will be created when jumpAheadTarget: labelSymbol is sent to self.  This and its corresponding target is only good for one use.  Other jumpAheadTo:... with the same label will be put on a stack and superceed existing ones until its jumpAheadTarget: is called."

	| instr |
	"jumpAheadTarget: label will pop this and replace destination with its basic block"
	(jumpAheadStacks at: labelSymbol ifAbsentPut: [OrderedCollection new])
		addLast: (instr := self add: (IRJumpIf new boolean: boolean)).
	self startNewSequence.
	instr otherwise: currentSequence.
