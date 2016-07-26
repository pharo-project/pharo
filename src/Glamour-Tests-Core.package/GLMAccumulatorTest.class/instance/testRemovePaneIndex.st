tests
testRemovePaneIndex
	| accumulator presentation |
	accumulator := GLMAccumulator new.
	presentation := GLMPresentation new.
	accumulator addPresentation: presentation.
	accumulator entity: 42.
	accumulator entity: 43.
	accumulator removePaneIndex: 2.
	self assert: accumulator panes size equals: 1