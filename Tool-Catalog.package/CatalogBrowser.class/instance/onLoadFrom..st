private - events
onLoadFrom: presenter
	presenter selection ifNil: [ ^self ].
	self loadConfiguration: presenter selection
		  onSuccess: [ presenter update ]