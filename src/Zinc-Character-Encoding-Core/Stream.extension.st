Extension { #name : 'Stream' }

{ #category : '*Zinc-Character-Encoding-Core' }
Stream >> rawStream [
	"Answer the innermost stream wrapped by the receiver, e.g. a raw binary file stream,
	socket stream, or regular Read/WriteStream.
	Since this the base stream, answer ourself."

	^ self
]

{ #category : '*Zinc-Character-Encoding-Core' }
Stream >> wrappedStream [
	"Answer the stream that the receiver wraps.
	Since this the base stream, answer nil"

	^ nil
]
