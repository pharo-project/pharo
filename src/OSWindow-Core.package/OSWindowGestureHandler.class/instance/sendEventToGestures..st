events
sendEventToGestures: anEvent
	registeredGestures
		do: [ :gesture | gesture handleEvent: anEvent ]