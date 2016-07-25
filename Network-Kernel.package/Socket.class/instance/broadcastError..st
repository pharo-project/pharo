private - errors
broadcastError: hostAddress
	^ (NoBroadcastAllowed new
		messageText: 'Sending to ' , hostAddress printString , ' without SO_BROADCAST set')
		signal