private
clientIPAddressFrom: event
	^ event request headers 
		at: ZnConstants remoteAddressHeader 
		ifAbsent: [ '-' ]