instructions
jumpBackTo: labelSymbol
	"Pop last remembered position with this label and write an unconditional jump to it"

	| sequence jump |
	sequence := (jumpBackTargetStacks at: labelSymbol ifAbsent: [self error: 'Missing jumpBackTarget: ', labelSymbol printString]) removeLast.
	jump := IRJump new destination: sequence.
	self add: jump.
	self startNewSequence.
	jump successor: currentSequence.
	