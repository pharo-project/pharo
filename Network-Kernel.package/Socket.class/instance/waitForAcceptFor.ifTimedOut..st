waiting
waitForAcceptFor: timeout ifTimedOut: timeoutBlock
	"Wait and accept an incoming connection"
	self waitForConnectionFor: timeout ifTimedOut: [^timeoutBlock value].
	^self accept