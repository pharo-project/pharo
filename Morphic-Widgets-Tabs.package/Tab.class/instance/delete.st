protocol
delete

	super delete.
	morph delete.
	retrievingProcess ifNotNil: [ retrievingProcess terminate ].
	self triggerEvent: #tabDeleted with: self