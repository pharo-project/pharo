initialization
forDebugger: aDebugger
	super forDebugger: aDebugger.
	
	SlReporter autoSubmit ifTrue: [
		self open.
		self enabled: false ].