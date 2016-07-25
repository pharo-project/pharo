serializing
serializeDefinitions: aCollection
	^String streamContents: [:aStream |
		| writer |
		writer := self snapshotWriterClass on: aStream.
		writer writeDefinitions: aCollection]