system changes
installSystemNotifications
	"Allows myself to be kept up-to-date regarding system changes"
	
	SystemAnnouncer uniqueInstance unsubscribe: self.
	
	SystemAnnouncer uniqueInstance weak
			when: ClassRemoved send: #classRemovedEventOccurs: to: self;
			when: MethodRemoved send: #removedEventOccurs: to: self;
			when: MethodAdded send: #addedEventOccurs: to: self;
			when: MethodModified send: #modifiedEventOccurs: to: self