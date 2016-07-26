once-reset
resetHaltOnCount
	 #haltOnCount: senders do: [ :method | method ast removeProperty: #haltCount ifAbsent: [  ] ].