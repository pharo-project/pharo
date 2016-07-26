operations
logOffEvent: anEvent
	"FIX"

	browser commentsLog 
		addEntryWith: anEvent
		tags: [ :tags |
			tags at: EpLog authorKey put: EpMonitor current author.
			tags at: EpLog timeKey put: EpMonitor current time ].

	browser refresh. "FIX: this could be done via announcement subscription"