icons
exclamationIcon 

	^  icons
			at: #'exclamationIcon'
			ifAbsentPut:[ Form fromBinaryStream: (Base64MimeConverter mimeDecodeToBytes: self  exclamationIconContents readStream) ].
