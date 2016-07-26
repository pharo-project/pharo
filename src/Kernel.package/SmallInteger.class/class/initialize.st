initialize-release
initialize
	SessionManager default
		registerSystemClassNamed: self name
		atPriority: 10.
	self startUp: true