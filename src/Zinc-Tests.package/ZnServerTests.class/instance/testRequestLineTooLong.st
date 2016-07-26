testing
testRequestLineTooLong
	self usingClassicSocketStreamsOnWindowsDo: [
		self withServerDo: [ :server |
		self deny: server debugMode.
		self assertRequestLineTooLong: server localUrl ] ]