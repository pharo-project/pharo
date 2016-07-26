instance creation
new: size

	^self new: size chunkSize: self defaultChunkSize
