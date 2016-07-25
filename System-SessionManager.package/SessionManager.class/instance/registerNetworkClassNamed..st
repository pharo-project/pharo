registration
registerNetworkClassNamed: aClassName
	self 
		register: (ClassSessionHandler forClassNamed: aClassName)
		inCategory: self networkCategory 
		atPriority: self defaultPriority