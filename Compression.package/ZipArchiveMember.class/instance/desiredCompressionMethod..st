accessing
desiredCompressionMethod: aNumber
	"Set my desiredCompressionMethod
	This is the method that will be used to write.
	Answers prior desiredCompressionMethod.

	Only CompressionDeflated or CompressionStored are valid arguments.

	Changing to CompressionStored will change my desiredCompressionLevel
	to CompressionLevelNone; changing to CompressionDeflated will change my
	desiredCompressionLevel to CompressionLevelDefault."

	| old |
	self flag: 'should be an error if unsupported, but valid zip compression is requested. Also, clean up test branches here'.
	old := desiredCompressionMethod.
	desiredCompressionMethod := aNumber.
	desiredCompressionLevel := (aNumber = CompressionDeflated)
			ifTrue: [ CompressionLevelDefault ]
			ifFalse: [ CompressionLevelNone ].
	compressionMethod = CompressionStored ifTrue: [ compressedSize := uncompressedSize ].
	^old.