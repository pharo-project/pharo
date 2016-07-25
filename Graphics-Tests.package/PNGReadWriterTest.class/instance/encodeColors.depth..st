tests - decoding
encodeColors: colorsAndFiles depth: requiredDepth
	
	colorsAndFiles do:[:assoc| | original encoded color ff |
		color := assoc key.
		original := Base64MimeConverter mimeDecodeToBytes: assoc value readStream.
		ff := Form extent: 32@32 depth: requiredDepth.
		ff fillColor: color.
		encoded := ByteArray new writeStream.
		PNGReadWriter putForm: ff onStream: encoded.
		self assert: (encoded contents = original contents).
	].