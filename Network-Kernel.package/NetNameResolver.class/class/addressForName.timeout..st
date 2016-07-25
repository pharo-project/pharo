lookups
addressForName: hostName timeout: secs
	"Look up the given host name and return its address. Return nil if the address is not found in the given number of seconds."
	"NetNameResolver addressForName: 'create.ucsb.edu' timeout: 30"
	"NetNameResolver addressForName: '100000jobs.de' timeout: 30"
	"NetNameResolver addressForName: '1.7.6.4' timeout: 30"
	"NetNameResolver addressForName: '' timeout: 30"

	| deadline result |
	self initializeNetwork.
	"check if this is a valid numeric host address (e.g. 1.2.3.4)"
	result := self addressFromString: hostName.
	result ifNotNil: [ ^ result asSocketAddress ].

	"Look up a host name, including ones that start with a digit (e.g. 100000jobs.de or www.pharo-project.org)"
	deadline := Time millisecondClockValue -> (secs * 1000).
	"Protect the execution of this block, as the ResolverSemaphore is used for both parts of the transaction."
	self resolverMutex
		critical: [
			(self waitForResolverReadyUntil: deadline)
				ifTrue: [
					self primStartLookupOfName: hostName.
					(self waitForCompletionUntil: deadline)
						ifTrue: [ result := self primNameLookupResult ]
						ifFalse: [ ^ NameLookupFailure signalFor: hostName ] ]
				ifFalse: [ ^ NameLookupFailure signalFor: hostName ] ].
	^ result asSocketAddress