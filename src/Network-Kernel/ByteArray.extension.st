Extension { #name : #ByteArray }

{ #category : #'*Network-Kernel' }
ByteArray >> asSocketAddress [
	^SocketAddress fromOldByteAddress: self
]
