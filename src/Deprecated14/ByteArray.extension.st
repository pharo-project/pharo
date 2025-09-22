Extension { #name : 'ByteArray' }

{ #category : '*Deprecated14' }
ByteArray >> asSocketAddress [

	self
		deprecated: 'Please use SocketAddress>>fromOldByteAddress: instead'
		transformWith: '`@receiver asSocketAddress' -> 'SocketAddress fromOldByteAddress: `@receiver'.
	^ SocketAddress fromOldByteAddress: self
]
