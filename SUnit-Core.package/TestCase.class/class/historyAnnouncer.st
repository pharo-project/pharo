accessing
historyAnnouncer
	^ HistoryAnnouncer ifNil: [  HistoryAnnouncer := Announcer new ]