private
startNewSequence
	"End current instruction sequence and start a new sequence to add instructions to.  If ending block just falls through to new block then add an explicit jump to it so they stay linked"

	| newSequence | 
	currentSequence ifEmpty: [^ self].	"block is still empty, continue using it"
	newSequence := IRSequence orderNumber: currentSequence orderNumber + 1.
	newSequence method: ir.

	currentSequence last transitionsToNextSequence
		ifFalse: [ self add: (IRJump new destination: newSequence)].
	self currentScope isPushClosureCopy ifTrue: [self currentScope lastBlockSequence: currentSequence].
	currentSequence := newSequence.
