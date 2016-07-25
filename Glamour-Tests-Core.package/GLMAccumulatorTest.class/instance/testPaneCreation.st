tests
testPaneCreation
	| accumulator presentation |
	accumulator := GLMAccumulator new.
	presentation := GLMPresentation new.
	accumulator addPresentation: presentation.
	accumulator entity: 41.
	self assert: accumulator panes size equals: 1.
	self assert: (accumulator panes first port: #entity) value equals: 41.
	self assert: accumulator panes first presentations first rootPrototype == presentation.
	accumulator entity: 42.
	self assert: accumulator panes size equals: 2.
	self assert: (accumulator panes first port: #entity) value equals: 41.
	self assert: (accumulator panes last port: #entity) value equals: 42.
	self assert: accumulator panes first presentations first rootPrototype equals: presentation.
	self assert: accumulator panes last presentations first rootPrototype equals: presentation