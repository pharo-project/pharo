Extension { #name : 'SocketStream' }

{ #category : '*Zinc-Character-Encoding-Core' }
SocketStream >> rawStream [
	"Answer the innermost stream wrapped by the receiver, e.g. a raw binary file stream,
	socket stream, or regular Read/WriteStream.
	Since this the base stream, answer ourself."

	^ self
]

{ #category : '*Zinc-Character-Encoding-Core' }
SocketStream >> wrappedStream [
	"Answer the stream that the receiver wraps.
	Since this the base stream, answer nil"

	^ nil
]
