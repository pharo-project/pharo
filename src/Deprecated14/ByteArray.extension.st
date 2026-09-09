Extension { #name : 'ByteArray' }

{ #category : '*Deprecated14' }
ByteArray >> asSocketAddress [

	self
		deprecated: 'Please use SocketAddress>>fromOldByteAddress: instead'
		transformWith: '`@receiver asSocketAddress' -> 'SocketAddress fromOldByteAddress: `@receiver'.
	^ SocketAddress fromOldByteAddress: self
]

{ #category : '*Deprecated14' }
ByteArray >> readStringUTF8 [

	self
		deprecated: 'Please use #utf8StringFromCString instead'
		transformWith: '`@receiver readStringUTF8'
						-> '`@receiver utf8StringFromCString'.

	^ (ExternalData fromHandle: self type: ExternalType string)
		  utf8StringFromCString
]
