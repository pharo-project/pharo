accessing
collectionSpecies
	"The type of collection returned by the stream"

	^binary ifTrue: [ByteArray] ifFalse: [ByteString]