tests
testPresentationBoundPort
	| presentation port |
	presentation := GLMPresentation new.
	port := (GLMPresentationBoundPort new)
		name: #port;
		presentation: presentation.
	port value: 42.
	self assert: (presentation pane port: #port) value equals: 42.
	(presentation pane port: #port) value: 43.
	self assert: port value equals: 43