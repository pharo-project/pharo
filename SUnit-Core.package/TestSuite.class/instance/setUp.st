running
setUp
	self resources  do: [ :each |
			each isAvailable ifFalse: [ each signalInitializationError ]
		].