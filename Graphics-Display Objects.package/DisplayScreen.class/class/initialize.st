initialize-release
initialize
	self deferUpdates: false.
	SessionManager default 
		registerGuiClassNamed: self name
		atPriority: 10.