initialization
fromRfc822: aString 
	"Parse aString to initialize myself."
	| parseStream contentType bodyText contentTransferEncoding |
	text := aString trimRight, String cr.
	parseStream := text readStream.
	contentType := 'text/plain'.
	contentTransferEncoding := nil.
	fields := Dictionary new.

	"Extract information out of the header fields"
	self 
		fieldsFrom: parseStream
		do: 
			[ :fName :fValue | 
			"NB: fName is all lowercase"
			fName = 'content-type' ifTrue: [ contentType := (fValue copyUpTo: $;) asLowercase ].
			fName = 'content-transfer-encoding' ifTrue: [ contentTransferEncoding := fValue asLowercase ].
			(fields 
				at: fName
				ifAbsentPut: [ OrderedCollection new: 1 ]) add: (MIMEHeaderValue 
					forField: fName
					fromString: fValue) ].

	"Extract the body of the message"
	bodyText := parseStream upToEnd.
	contentTransferEncoding = 'base64' ifTrue: 
		[ bodyText := Base64MimeConverter mimeDecodeToChars: bodyText readStream.
		bodyText := bodyText contents ].
	contentTransferEncoding = 'quoted-printable' ifTrue: [ bodyText := bodyText decodeQuotedPrintable ].
	body := MIMEDocument 
		contentType: contentType
		content: bodyText