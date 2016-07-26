registration
registerSystemClassNamed: aClassName atPriority: anInteger
	self 
		register: (ClassSessionHandler forClassNamed: aClassName)
		inCategory: self systemCategory 
		atPriority: anInteger