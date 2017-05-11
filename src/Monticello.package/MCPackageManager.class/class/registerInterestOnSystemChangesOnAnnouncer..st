event registration
registerInterestOnSystemChangesOnAnnouncer: anAnnouncer
	anAnnouncer weak
		when: RPackageRenamed send: #packageRenamed: to: self;
		when: ClassAdded, ClassModifiedClassDefinition, ClassCommented , ClassParentRenamed send: #classModified: to: self;
		when: ClassRenamed send:#classRenamed: to:self;
		when: ClassRepackaged send: #classMoved: to: self;
		when: ClassRemoved send: #classRemoved: to: self;
		when: MethodAdded, MethodModified, MethodRecategorized send: #methodModified: to: self;
		when: MethodRepackaged send: #methodMoved: to: self;
		when: MethodRemoved send: #methodRemoved: to: self.