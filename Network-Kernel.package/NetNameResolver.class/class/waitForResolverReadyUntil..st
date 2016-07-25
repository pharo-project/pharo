private
waitForResolverReadyUntil: deadline
	"Wait up to the given number of seconds for the resolver to be ready to accept a new request. Return true if the resolver is not busy, false if the network is not initialized or the resolver does not become free within the given time period."

	| status |
	status := self resolverStatus.
	status = ResolverUninitialized ifTrue: [^ false].
	status := self waitForResolverNonBusyUntil: deadline.
	^ status ~= ResolverBusy
