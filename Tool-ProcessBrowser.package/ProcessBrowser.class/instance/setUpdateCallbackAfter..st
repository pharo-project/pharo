updating
setUpdateCallbackAfter: seconds
	deferredMessageRecipient
		ifNotNil: [ 
			| d |
			d := Delay forSeconds: seconds.
			[ d wait.
			d := nil. 
			deferredMessageRecipient defer: [ self updateProcessList ] ] fork ]