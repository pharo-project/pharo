once-reset
resetOnce
	 (#haltOnce senders, #inspectOnce senders) do: [ :method | 
		method ast sendNodes do: [ :sendNode| 
			sendNode removeProperty: #Once ifAbsent: [  ]]]