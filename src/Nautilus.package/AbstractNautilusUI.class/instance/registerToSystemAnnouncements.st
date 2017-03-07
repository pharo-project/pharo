announcement registration
registerToSystemAnnouncements
	self browsedEnvironment packageOrganizer announcer weak
		when: RPackageRegistered send: #packageRegistered: to: self;
		when: RPackageUnregistered send: #packageUnregistred: to: self;
		when: RPackageRenamed send: #packageRenamed: to: self.
						
	SystemAnnouncer uniqueInstance weak
		when: MCPackageModified send: #mcPackageModified: to: self;
		when: MCWorkingCopyCreated send: #mcWorkingCopyCreated: to: self;
		when: MCWorkingCopyDeleted send: #mcWorkingCopyDeleted: to: self;
		when: ClassAdded send: #classAdded: to: self;
		when: ClassCommented send: #classCommented: to: self;
		when: ClassRecategorized send: #classRecategorized: to: self;
		when: ClassModifiedClassDefinition send: #classDefinitionModified: to: self;
		when: ClassParentRenamed send: #classParentRenamed: to: self;
		when: ClassRemoved send: #classRemoved: to: self;
		when: ClassRenamed send: #classRenamed: to: self;
		when: ClassReorganized send: #classReorganized: to: self;
		when: MethodAdded send: #methodAdded: to: self;
		when: MethodModified send: #methodModified: to: self;
		when: MethodRecategorized send: #methodRecategorized: to: self;
		when: MethodRemoved send: #methodRemoved: to: self;
		when: MetalinkChanged send: #metaLinkModified: to: self.
