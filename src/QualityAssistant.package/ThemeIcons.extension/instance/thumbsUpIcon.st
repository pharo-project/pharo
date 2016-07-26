*qualityAssistant
thumbsUpIcon
	^ icons
			at: #thumbsUpIcon
			ifAbsentPut: [ Form fromBinaryStream: (Base64MimeConverter mimeDecodeToBytes: self thumbsUpIconContents readStream) ].