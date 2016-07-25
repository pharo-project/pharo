subscription
whenChangedSend: aSelector to: anObject
	"record a change listener"
	
	self when: PragmaAnnouncement send: aSelector to: anObject
