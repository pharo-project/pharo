ssl
accept
	"Do the SSL server handshake."
	
	| count result |
	self resetEncryptedBuffers.
	count := 0.
	connecting := true.
	[ self sslSession isConnected ] whileFalse: [
		count := self readEncryptedBytes: in startingAt: 1 count: in size.
		result := self sslSession accept: in from: 1 to: count into: out.
		result < -1 ifTrue: [ 
			^ self sslException: 'accept failed' code: result ].
		result > 0 ifTrue: [ 
			self flushEncryptedBytes: out startingAt: 1 count: result ] ].
	connecting := false