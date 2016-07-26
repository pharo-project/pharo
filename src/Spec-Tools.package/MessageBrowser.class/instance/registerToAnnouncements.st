announcements-registration
registerToAnnouncements

	SystemAnnouncer uniqueInstance weak		
		when: MethodAdded send: #methodAdded: to: self;
		when: MethodModified send: #methodModified: to: self;
		when: MethodRecategorized send: #methodRecategorized: to: self;
		when: MethodRemoved send: #methodRemoved: to: self;
		when: ClassRenamed send: #classRenamed: to: self