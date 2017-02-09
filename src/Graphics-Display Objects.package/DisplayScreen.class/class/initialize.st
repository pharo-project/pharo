initialize-release
initialize
	| display |
	display := self new.
	display
		setExtent: self actualScreenSize
		depth: 32. 
	Smalltalk globals at: #Display put: display.
	display beDisplay.
	
	self deferUpdates: false.
	SessionManager default 
		registerGuiClassNamed: self name
		atPriority: 10.