private - subscription
subscribeToLog

	self log announcer weak
		when: EpEntryAdded
		send: #refresh
		to: self