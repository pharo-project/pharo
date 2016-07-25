serializing
serialize: anObject
	"Serialize the graph starting at the root object received and answers the FLSerialization object"

	^ self serialize: anObject on: stream