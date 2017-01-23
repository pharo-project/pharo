tests
testAnnouncement

	| wasAnnounced |
	wasAnnounced := false.

	store announcer
		subscribe: OmSessionStoreUpdated
		do: [ :announcement | wasAnnounced := true ].
	
	[ store resetWithStoreNamed: 'new' ] 
		ensure: [ store announcer unsubscribe: self ].

	self assert: wasAnnounced
	