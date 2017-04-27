menu - operations
logOffEvent: anEvent

	self commentsLog 
		addEntryWith: anEvent
		tags: [ :tags |
			tags at: EpLog authorKey put: self monitor author.
			tags at: EpLog timeKey put: self monitor time ].