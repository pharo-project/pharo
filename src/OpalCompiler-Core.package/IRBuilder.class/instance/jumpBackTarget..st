instructions
jumpBackTarget: labelSymbol
	"Remember this basic block for a future jumpBackTo: labelSymbol.  Stack up remembered targets with same name and remove them from stack for each jumpBackTo: called with same name."

	self startNewSequence.
	(jumpBackTargetStacks at: labelSymbol ifAbsentPut: [OrderedCollection new])
		addLast: currentSequence.
