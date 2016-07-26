initialization
registerToAnnouncements

	SystemAnnouncer uniqueInstance unsubscribe: self.
	SystemAnnouncer uniqueInstance weak
		"Class"
		when: ClassCommented send: #aClassHasBeenCommented: to: self;
		when: ClassRemoved send: #aClassHasBeenRemoved: to: self;
		when: ClassRenamed send: #aClassHasBeenRenamed: to: self;
		"Method"
		when: MethodAdded send: #aMethodHasBeenAdded: to: self;
		when: MethodModified send: #aMethodHasBeenModified: to: self;
		when: MethodRemoved send: #aMethodHasBeenRemoved: to: self