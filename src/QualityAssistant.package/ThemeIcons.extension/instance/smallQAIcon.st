*qualityAssistant
smallQAIcon
	^ icons
			at: #'smallQAIcon'
			ifAbsentPut: [ Form fromBinaryStream: (Base64MimeConverter mimeDecodeToBytes: self smallQAIconContents readStream) ].