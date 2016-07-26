private-announcers
startSystemAnnouncements
	systemAnnouncements == true ifTrue: [ ^ self ]. " handle: nil | false | true / avoid multiple subscriptions "
	self announcer
		when: MethodAdded send: #methodAdded: to: self;
		when: MethodModified send: #methodModified: to: self;
		when: MethodRemoved send: #methodRemoved: to: self;
		when: ClassRemoved send: #classRemoved: to: self;
		when: ClassRenamed send: #classRenamed: to: self;
		when: CategoryRemoved send: #categoryRemoved: to: self.
	systemAnnouncements := true