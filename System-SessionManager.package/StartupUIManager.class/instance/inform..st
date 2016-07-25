ui requests
inform: aString
	(ProvideAnswerNotification signal: aString) 
		ifNotNil: [:answer | ^true].
	self sessionManager 
		addDeferredStartupAction: [ UIManager default inform: aString ].