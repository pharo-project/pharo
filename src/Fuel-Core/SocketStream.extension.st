Extension { #name : 'SocketStream' }

{ #category : '*Fuel-Core' }
SocketStream >> nextBytesPutAll: aCollection [
	"Append the bytes of aCollection to the sequence of bytes accessible 
	by the receiver. Answer aCollection."

	^ self nextPutAll: aCollection
]
