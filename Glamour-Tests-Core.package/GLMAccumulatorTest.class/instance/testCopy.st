tests
testCopy
	| accumulator presentation copiedAccumulator |
	accumulator := GLMAccumulator new.
	presentation := GLMPresentation new.
	accumulator addPresentation: presentation.
	(accumulator transmit)
		fromOutsidePort: #selection;
		toOutsidePort: #otherPort.
	copiedAccumulator := accumulator copy.
	self assert: copiedAccumulator transmissions size equals: 1.
	self assert: copiedAccumulator transmissions first ~~ accumulator transmissions first.
	self assert: copiedAccumulator transmissions first destination pane == copiedAccumulator pane.
	self assert: copiedAccumulator transmissions first origins first pane ~~ accumulator pane.
	self assert: copiedAccumulator transmissions first origins first pane == copiedAccumulator pane