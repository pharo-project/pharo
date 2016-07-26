packing
unpackFile: aFileReference
	"self default unpackFile: './gt/events/' asFileReference allFiles atRandom"
	^ [ self unpack: (aFileReference binaryReadStreamDo: [ :aStream |  aStream contents ]) ]
		on: GTEventUnpackingError do: [ :anException |
			anException data: aFileReference.
			anException pass ]