initialization
initializeWorkaroundToRefreshOnMonitorLogAnnouncement

	self flag: #todo. "Fix"
	monitorLog := EpMonitor current log.
	refreshDeferrer := OmDeferrer 
		send: #refreshIfMonitorLogSelected
		to: self
		after: 500 milliSeconds.

	monitorLog announcer weak
		when: EpEntryAdded
		send: #schedule
		to: refreshDeferrer.

	self whenWindowChanged: [ :windowModel | 
		windowModel whenClosedDo: [ 
			monitorLog announcer unsubscribe: refreshDeferrer ] ].