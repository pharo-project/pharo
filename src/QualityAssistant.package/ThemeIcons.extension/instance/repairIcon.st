*qualityAssistant
repairIcon
	^ icons
			at: #'repairIcon'
			ifAbsentPut: [ Form fromBinaryStream: (Base64MimeConverter mimeDecodeToBytes: self repairIconContents readStream) ].