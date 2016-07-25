registration
registerSystemClassNamed: aClassName
	self 
		register: (ClassSessionHandler forClassNamed: aClassName)
		inCategory: self systemCategory 
		atPriority: self defaultPriority