user interface
whenPrivateChangedSend: zeroOrOneArgSelector to: aSubscriber 
	"private, use #whenChangedSend:to: instead"
	self
		when: self privateChangeEvent
		send: zeroOrOneArgSelector
		to: aSubscriber