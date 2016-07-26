tests
testPublishToOutside
	| accumulator presentation |
	accumulator := GLMAccumulator new.
	presentation := GLMPresentation new.
	accumulator addPresentation: presentation.
	accumulator startOn: 41.
	(accumulator panes first port: #selection) value: 42.
	self assert: (accumulator pane port: #selection) value equals: 42