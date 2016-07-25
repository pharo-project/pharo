accessing
desiredCompressionLevel: aNumber
	"Set my desiredCompressionLevel
	This is the method that will be used to write.
	Returns prior desiredCompressionLevel.

	Valid arguments are 0 (CompressionLevelNone) through 9,
	including 6 (CompressionLevelDefault).

	0 (CompressionLevelNone) will change the desiredCompressionMethod
	to CompressionStored. All other arguments will change the
	desiredCompressionMethod to CompressionDeflated."

	| old |
	old := desiredCompressionLevel.
	desiredCompressionLevel := aNumber.
	desiredCompressionMethod := (aNumber > 0)
		ifTrue: [ CompressionDeflated ]
		ifFalse: [ CompressionStored ].
	^old