subscription
whenResetSend: aSelector to: anObject
	"record a change listener"

	self when: PragmaCollectorReset send: aSelector to: anObject