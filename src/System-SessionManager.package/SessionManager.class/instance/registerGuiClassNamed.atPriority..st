registration
registerGuiClassNamed: aClassName atPriority: anInteger
	self 
		register: (ClassSessionHandler forClassNamed: aClassName)
		inCategory: self guiCategory 
		atPriority: anInteger