protocol
delete

	super delete.
	morph ifNotNil: [morph delete].
	retrievingProcess ifNotNil: [ retrievingProcess terminate ].
	self triggerEvent: #tabDeleted with: self