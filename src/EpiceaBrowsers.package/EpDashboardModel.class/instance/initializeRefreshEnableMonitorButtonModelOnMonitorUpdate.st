initialization
"protocol: initialization"
initializeRefreshEnableMonitorButtonModelOnMonitorUpdate

	| monitorAnnouncer |
	monitorAnnouncer := self monitor announcer.

	monitorAnnouncer weak
		when: EpMonitorStateUpdated 
		send: #refreshEnableMonitorButtonModel
		to: self.

	self whenWindowChanged: [ :windowModel | 
		windowModel whenClosedDo: [ 
			monitorAnnouncer unsubscribe: self ] ].
