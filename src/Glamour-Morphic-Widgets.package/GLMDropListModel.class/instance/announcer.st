accessing
announcer
	^ announcer ifNil: [announcer := GLMAnnouncer new]