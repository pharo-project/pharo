cleanup
close
	"Close any HTTP network connection that I might have kept open."

	connection
		ifNotNil: [ 
			| address port |
			address := [ connection socket remoteAddress ] on: Error do: [ nil ].
			port := [ connection socket remotePort ] on: Error do: [ nil ].
			connection close.
			connection := nil.
			self logConnectionClosedAddress: address port: port ].
	lastUsed := nil