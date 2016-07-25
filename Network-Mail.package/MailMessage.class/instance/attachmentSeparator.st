multipart
attachmentSeparator
	^(self fieldNamed: 'content-type' ifAbsent: [^nil]) parameters
		at: 'boundary' ifAbsent: [^nil]