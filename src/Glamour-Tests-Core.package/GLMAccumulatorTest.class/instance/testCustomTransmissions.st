tests
testCustomTransmissions
	| accumulator presentation |
	accumulator := GLMAccumulator new.
	presentation := GLMPresentation new.
	accumulator addPresentation: presentation.
	(accumulator transmit)
		fromOutsidePort: #selection;
		toOutsidePort: #otherPort.
	accumulator startOn: 41.
	(accumulator panes first port: #selection) value: 42.
	self assert: (accumulator pane port: #otherPort) value equals: 42