tests
testNoDuplicatePaneCreation
	| accumulator presentation |
	accumulator := GLMAccumulator new.
	presentation := GLMPresentation new.
	accumulator addPresentation: presentation.
	accumulator entity: 41.
	self assert: accumulator panes size equals: 1.
	accumulator entity: 42.
	self assert: accumulator panes size equals: 2.
	accumulator entity: 41.
	self assert: accumulator panes size equals: 2