tests - decoding
decodeColors: colorsAndFiles depth: requiredDepth
	
	colorsAndFiles do:[:assoc| | bytes color form |
		color := assoc key.
		bytes := Base64MimeConverter mimeDecodeToBytes: assoc value readStream.
		form := PNGReadWriter formFromStream: bytes.
		self assert: form depth = requiredDepth.
		self assert: (form pixelValueAt: 1@1) = (color pixelValueForDepth: requiredDepth).
	].