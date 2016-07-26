private - errors
broadcastMisconfiguredForSendingTo: hostAddress
	^ (self isBroadcastAddress: hostAddress)
		and: [ ((self getOption: 'SO_BROADCAST') last = 0) ]