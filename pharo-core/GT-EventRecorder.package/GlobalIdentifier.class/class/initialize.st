class initialization
initialize

	SessionManager default
		register: (ClassSessionHandler forClassNamed: self name)
		inCategory: SessionManager default systemCategory  
		atPriority: SessionManager default defaultPriority - 100.