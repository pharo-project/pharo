system annoucements
registerToAnnouncements
	
	SystemAnnouncer uniqueInstance weak
		when: ClassModifiedClassDefinition send: #classModified: to: self;
		when: MethodModified send: #methodModified: to: self;
		when: MethodRemoved send: #methodRemoved: to: self.
	self window window announcer when: WindowClosed send: #onWindowClosed to: self.	