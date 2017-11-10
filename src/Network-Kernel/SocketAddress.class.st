"
I represent a socket (network) address consisting of a host internet address and a port number.  My contents are opaque and cannot be interpreted directly.  See the accessing protocol for methods that retrieve the information I contain.
"
Class {
	#name : #SocketAddress,
	#superclass : #ByteArray,
	#type : #bytes,
	#category : #Network-Kernel
}

{ #category : #'instance creation' }
SocketAddress class >> fromOldByteAddress: byteArray [
	^self newFrom: byteArray
]

{ #category : #converting }
SocketAddress >> asSocketAddress [

]
