private
waitForResolverNonBusyUntil: deadline
	"Wait up to the given number of seconds for the resolver to be non busy.
	Return the resolver state."

	| status passed |
	status := self resolverStatus.
	[ status = ResolverBusy and: [ (passed := Time millisecondsSince: deadline key) < deadline value] ]
		whileTrue: [
			"wait for resolver to be available"
			ResolverSemaphore waitTimeoutMSecs: (deadline value - passed).
			status := self resolverStatus ].
	^ status