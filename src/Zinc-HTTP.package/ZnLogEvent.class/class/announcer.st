accessing
announcer
	^ LogEventAnnouncer ifNil: [ LogEventAnnouncer := Announcer new ]