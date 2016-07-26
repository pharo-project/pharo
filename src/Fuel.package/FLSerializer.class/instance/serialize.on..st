serializing
serialize: anObject on: aStream
	"Serialize the graph starting at the root object received and answers the FLSerialization object"

	FLEncoder
		on: aStream 
		globalEnvironment: self analyzer globalEnvironment
		do: [ :anEncoder | 
			self encodeSignatureWith: anEncoder.
			self encodeVersionWith: anEncoder.
			self encodeHeaderWith: anEncoder.
			^ self serializationFactory value: anObject value: anEncoder ]