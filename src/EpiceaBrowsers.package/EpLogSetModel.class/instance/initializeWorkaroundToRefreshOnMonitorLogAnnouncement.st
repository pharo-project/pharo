initialization
initializeWorkaroundToRefreshOnMonitorLogAnnouncement

	self flag: #todo. "Fix"
	monitorLog := EpMonitor current log.

	monitorLog announcer weak
		when: EpEntryAdded
		send: #refreshIfMonitorLogSelected
		to: self.

	self whenWindowChanged: [ :windowModel | 
		windowModel whenClosedDo: [ 
			monitorLog announcer unsubscribe: self ] ].