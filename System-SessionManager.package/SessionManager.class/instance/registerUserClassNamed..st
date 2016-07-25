registration
registerUserClassNamed: aClassName
	self 
		register: (ClassSessionHandler forClassNamed: aClassName)
		inCategory: self userCategory 
		atPriority: self defaultPriority