private - errors
errorSending: data startingAt: startIndex count: aCount toHost: hostAddress port: portNumber
	"If there are known primitive failure reasons when sending that can be specified using more exact errors, discern them here"
"Only applicable to UDP sockets, TCP does not have broadcast"
	(self broadcastMisconfiguredForSendingTo: hostAddress)
		ifTrue: [ ^ self broadcastError: hostAddress ].	
	^ self primitiveFailed: thisContext sender selector