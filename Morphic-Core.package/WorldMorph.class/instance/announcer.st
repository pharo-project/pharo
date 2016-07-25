accessing
announcer
	WorldAnnouncer ifNil: [ WorldAnnouncer := Announcer new ].
	^ WorldAnnouncer