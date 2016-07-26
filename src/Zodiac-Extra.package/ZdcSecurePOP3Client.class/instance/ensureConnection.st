private
ensureConnection
	"Overwritten & refactored"
		
	self isConnected ifTrue: [ ^ self ].
	self stream ifNotNil: [ self stream close ].

	self setupStream.
	self checkResponse.
	self login