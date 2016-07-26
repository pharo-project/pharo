tests
testPortEquality
	| presentation port |
	presentation := GLMPresentation new.
	port := (GLMPresentationBoundPort new)
		name: #port;
		presentation: presentation.
	self assert: port equals: (presentation pane port: #port).
	self assert: (presentation pane port: #port) equals: port.
	self assert: port hash equals: (presentation pane port: #port) hash.
	self assert: (presentation pane port: #port) hash equals: port hash