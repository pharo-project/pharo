instructions
jumpAheadTarget: labelSymbol
	"Pop latest jumpAheadTo: with this labelSymbol and have it point to this new instruction sequence"

	| jumpInstr |
	self startNewSequence.
	jumpInstr := (jumpAheadStacks at: labelSymbol ifAbsent: [
			self error: 'Missing jumpAheadTo: ', labelSymbol printString]) removeLast.
	jumpInstr destination: currentSequence.
