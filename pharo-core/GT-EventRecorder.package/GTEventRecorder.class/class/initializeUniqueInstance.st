initialization
initializeUniqueInstance
	uniqueInstance ifNotNil: [ ^ uniqueInstance ].
	uniqueInstance := self new.
	self registerInterestToSystemAnnouncement.
	^ uniqueInstance