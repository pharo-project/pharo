*qualityAssistant
thumbsDownIcon
	^ icons
			at: #thumbsDownIcon
			ifAbsentPut: [ Form fromBinaryStream: (Base64MimeConverter mimeDecodeToBytes: self tumbsDownIconContents readStream) ].