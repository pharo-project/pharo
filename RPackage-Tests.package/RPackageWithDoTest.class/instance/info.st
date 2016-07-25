tests
info
	
	
	self announcer hasSubscriber: RPackageOrganizer default.
	Smalltalk garbageCollect.
	RPackageOrganizer allInstances collect: [:each | self announcer hasSubscriber: each ] 