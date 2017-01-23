initialization
initializeRefreshOnSessionUpdate

	| updatesAnnouncer |
	updatesAnnouncer := EpMonitor current sessionStore announcer.

	updatesAnnouncer weak
		when: OmSessionStoreUpdated
		send: #refresh
		to: self.

	self whenWindowChanged: [ :windowModel | 
		windowModel whenClosedDo: [ 
			updatesAnnouncer unsubscribe: self ] ].