announcement registration
registerToGroupAnnouncements
	GroupAnnouncer uniqueInstance weak
		when: AGroupHasBeenAdded send: #aGroupHasBeenAdded: to: self;
		when: AGroupHasBeenRemoved send: #aGroupHasBeenRemoved: to: self;
		when: AGroupHasBeenRenamed send: #aGroupHasBeenRenamed: to: self;
		when: AGroupContentHasBeenModified send: #aGroupContentHasBeenModified: to: self