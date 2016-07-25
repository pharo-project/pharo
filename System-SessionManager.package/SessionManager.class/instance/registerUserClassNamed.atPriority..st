registration
registerUserClassNamed: aClassName atPriority: anInteger
	self 
		register: (ClassSessionHandler forClassNamed: aClassName)
		inCategory: self userCategory 
		atPriority: anInteger