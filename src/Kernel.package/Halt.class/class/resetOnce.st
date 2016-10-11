once-reset
resetOnce
	"Once stops an expression once and only once. Halt resetOnce will rearm and make sure that the execution of a method containing Halt once will stop another time."
	 (#haltOnce senders, #inspectOnce senders) do: [ :method | 
		method ast sendNodes do: [ :sendNode| 
			sendNode removeProperty: #Once ifAbsent: [  ]]]