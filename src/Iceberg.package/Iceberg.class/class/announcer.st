accessing
announcer
	^ IcebergAnnouncer ifNil: [ IcebergAnnouncer := Announcer new ]