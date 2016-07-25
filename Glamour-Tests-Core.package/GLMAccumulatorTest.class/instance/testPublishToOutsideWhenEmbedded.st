tests
testPublishToOutsideWhenEmbedded
	| browser accumulator actualAccumulator |
	browser := GLMTabulator new.
	accumulator := GLMAccumulator new.
	accumulator addPresentation: GLMPresentation new.
	accumulator
		addTransmission:
			((GLMTransmission new)
				addActiveOrigin:
						((GLMPresentationBoundPort new)
								presentation: accumulator;
								name: #selection;
								yourself);
				destination:
						((GLMPresentationBoundPort new)
								presentation: accumulator;
								name: #anotherPort;
								yourself);
				yourself).
	browser addColumn: #one.
	browser
		addTransmission:
			((GLMTransmission new)
				ensurePresentIfNoneStrategy;
				addActiveOrigin:
						((GLMPresentationBoundPort new)
								presentation: browser;
								name: #entity;
								yourself);
				destination: ((browser paneNamed: #one) port: #entity);
				addPresentation: accumulator;
				yourself).
	browser startOn: 41.
	actualAccumulator := browser panes first presentations first.
	(actualAccumulator panes first port: #selection) value: 42.
	self assert: (actualAccumulator pane port: #selection) value equals: 42.
	self assert: (actualAccumulator pane port: #anotherPort) value notNil.
	self assert: (actualAccumulator pane port: #anotherPort) value equals: 42