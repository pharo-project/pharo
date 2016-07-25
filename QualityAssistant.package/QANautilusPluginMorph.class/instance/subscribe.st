initialization
subscribe

	SystemAnnouncer uniqueInstance weak
		when: ClassCommented send: #classCommented: to: self;
		when: MethodRecategorized send: #methodModified: to: self.