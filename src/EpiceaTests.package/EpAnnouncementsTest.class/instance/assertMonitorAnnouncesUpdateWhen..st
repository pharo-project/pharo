tests
assertMonitorAnnouncesUpdateWhen: aBlock

	| wasAnnounced |
	wasAnnounced := false.

	monitor announcer
		subscribe: EpMonitorStateUpdated
		do: [ :announcement | wasAnnounced := true ].
	
	aBlock
		ensure: [ monitor announcer unsubscribe: self ].

	self assert: wasAnnounced
	