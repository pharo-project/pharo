registration
registerToolClassNamed: aClassName
	self 
		register: (ClassSessionHandler forClassNamed: aClassName)
		inCategory: self toolsCategory 
		atPriority: self defaultPriority