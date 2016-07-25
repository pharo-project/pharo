registration
registerGuiClassNamed: aClassName
	self 
		register: (ClassSessionHandler forClassNamed: aClassName)
		inCategory: self guiCategory 
		atPriority: self defaultPriority