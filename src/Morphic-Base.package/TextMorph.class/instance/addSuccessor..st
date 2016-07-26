linked frames
addSuccessor: evt
	| newMorph |
	newMorph := self copy predecessor: self successor: successor.
	newMorph extent: self width @ 100.
	successor ifNotNil: [successor setPredecessor: newMorph].
	self setSuccessor: newMorph.
	successor recomposeChain.
	evt hand attachMorph: newMorph