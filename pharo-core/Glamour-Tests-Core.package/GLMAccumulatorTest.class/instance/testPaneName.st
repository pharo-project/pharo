tests
testPaneName
	| accumulator presentation |
	accumulator := GLMAccumulator new.
	presentation := GLMPresentation new.
	accumulator addPresentation: presentation.
	accumulator entity: 41.
	self assert: accumulator panes first name equals: '1'.
	accumulator entity: 42.
	self assert: accumulator panes last name equals: '2'