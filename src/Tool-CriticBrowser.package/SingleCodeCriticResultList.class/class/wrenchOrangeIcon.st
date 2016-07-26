icons
wrenchOrangeIcon

	^  icons
			at: #'wrenchOrangeIcon'
			ifAbsentPut:[ Form fromBinaryStream: (Base64MimeConverter mimeDecodeToBytes: self  wrenchOrangeIconContents readStream) ].
