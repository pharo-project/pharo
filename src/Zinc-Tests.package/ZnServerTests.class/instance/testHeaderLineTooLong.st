testing
testHeaderLineTooLong
	self usingClassicSocketStreamsOnWindowsDo: [
		self withServerDo: [ :server |
			self deny: server debugMode.
			self assertHeaderLineTooLong: server localUrl ] ]