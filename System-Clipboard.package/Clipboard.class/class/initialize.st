initialization
initialize
	self startUp: true.
	SessionManager default 
		registerSystemClassNamed: self name