class initialization
initialize
	self useFilePlugin.
	SessionManager default
		registerSystemClassNamed: self name