waiting
waitForAcceptFor: timeout
	"Wait and accept an incoming connection. Return nil if it falis"
	self waitForConnectionFor: timeout ifTimedOut: [^ nil].
	^ self isConnected
		ifTrue:[self accept]
		