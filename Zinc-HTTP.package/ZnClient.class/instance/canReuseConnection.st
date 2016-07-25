private
canReuseConnection
	self isConnected ifFalse: [ ^ false ].
	lastUsed ifNil: [ ^ false ].
	^ Time totalSeconds - lastUsed < self connectionReuseTimeout