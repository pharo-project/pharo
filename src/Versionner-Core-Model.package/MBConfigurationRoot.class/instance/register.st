updating
register
	"Set up dependencies"
	
	SystemAnnouncer uniqueInstance weak
		when: ClassAdded send: #classAdded: to: self;
		when: ClassModifiedClassDefinition,
			ClassCommented,
			ClassRenamed, 
			ClassReorganized,
			ClassRenamed send: #classModified: to: self;
		when: ClassRemoved send: #classRemoved: to: self;		
		when: MethodAdded,
			MethodModified ,
			MethodRemoved send: #methodModified: to: self.
					
	MCWorkingCopy addDependent: self.
