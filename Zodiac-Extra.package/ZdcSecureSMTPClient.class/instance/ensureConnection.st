private
ensureConnection
	"Overwritten & refactored"
		
	self isConnected ifTrue: [ ^ self].
	self stream ifNotNil: [ self stream close ].
	self setupStream.
	"regarding RFC2487-5.2 the state after SSL negotiation is to be reset to the state after the
	220 server greeting. EHLO command is next which is issued in the following login method  "
	self login