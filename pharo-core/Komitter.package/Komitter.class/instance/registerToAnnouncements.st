announcement
registerToAnnouncements

	SystemAnnouncer uniqueInstance weak	
		when: MCPackageModified send: #rebuildStagingArea to: self;
		when: ClassAdded, ClassModifiedClassDefinition, ClassRenamed, ClassCommented send: #rebuildStagingArea to: self;
		when: ClassRepackaged send: #rebuildStagingArea to: self;
		when: ClassRemoved send: #rebuildStagingArea to: self;
		when: MethodAdded, MethodModified, MethodRecategorized send: #rebuildStagingArea to: self;
		when: MethodRepackaged send: #rebuildStagingArea to: self;
		when: MethodRemoved send: #rebuildStagingArea to: self