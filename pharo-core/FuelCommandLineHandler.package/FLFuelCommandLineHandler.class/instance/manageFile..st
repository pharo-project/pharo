manage
manageFile: aFileReference

	aFileReference readStreamDo: [ :stream |
		stream binary.
		FileStream stdout nextPutAll: 'Materializing from file: ', aFileReference fullName; lf.
		FLMaterializer new materializeFrom: stream.
		FileStream stdout nextPutAll: 'Materialization finished'; lf.
		].