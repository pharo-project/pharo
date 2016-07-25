class initialization
initialize
	SessionManager default
		register: (ClassSessionHandler forClassNamed: self name)
		inCategory: SessionManager default toolsCategory  
		atPriority: SessionManager default defaultPriority + 1000.
	self recordMeta.

	uniqueInstance := WeakArray new: 1.
	ReSystemAnnouncer uniqueInstance unsubscribe: self.
	ReSystemAnnouncer uniqueInstance weak when: ReCritiqueEvent send: #recordAnnouncement: to: self