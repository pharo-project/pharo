ssl
connect
	"Do the SSL client handshake."
	
	| count result |
	self resetEncryptedBuffers.
	count := 0.
	connecting := true.
	[ (result := self sslSession connect: in from: 1 to: count into: out) = 0 ] whileFalse: [
		result < -1 ifTrue: [ 
			^ self sslException: 'connect failed' code: result ].
		result > 0 ifTrue: [ 
			self flushEncryptedBytes: out startingAt: 1 count: result ].
		count := self readEncryptedBytes: in startingAt: 1 count: in size ].
	connecting := false
	