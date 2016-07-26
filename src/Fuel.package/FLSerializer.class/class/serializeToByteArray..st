serializing-shortcuts
serializeToByteArray: anObject
	"No stream is needed by the user. An internal in-memory stream will be used. This method returns a ByteArray representing the serialization"

	| aStream |
	aStream := WriteStream on: (ByteArray new: 100).
	self newDefault serialize: anObject on: aStream.
	^ aStream contents