private-writing
writeDataTo: aStream from: start to: finish
	"Copy my (possibly inflated or deflated) data to the given stream.
	But only the specified byte range.
	This might do decompression, or straight copying, depending
	on the values of compressionMethod and desiredCompressionMethod"

	uncompressedSize = 0 ifTrue: [ ^self ].	"nothing to do because no data"
	start > finish ifTrue: [ ^self ].
	start > uncompressedSize ifTrue: [ ^self ].

	(compressionMethod = CompressionStored and: [ desiredCompressionMethod = CompressionDeflated ])
		ifTrue: [ ^self error: 'only supports uncompression or copying right now' ].

	(compressionMethod = CompressionDeflated and: [ desiredCompressionMethod = CompressionStored ])
		ifTrue: [ ^self uncompressDataTo: aStream from: start to: finish ].

	self copyRawDataTo: aStream from: start to: finish.