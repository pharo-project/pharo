printing/formatting
regenerateText
	"regenerate the full text from the body and headers"
	
	text := String streamContents: 
		[ :str | | encodedBodyText | 
		"first put the header"
		fields keysAndValuesDo: 
			[ :fieldName :fieldValues | 
			fieldValues do: 
				[ :fieldValue | 
				str
					nextPutAll: fieldName capitalized;
					nextPutAll: ': ';
					nextPutAll: fieldValue asHeaderValue;
					cr ] ].

		"skip a line between header and body"
		str cr.

		"put the body, being sure to encode it according to the header"
		encodedBodyText := body content.
		self decoderClass ifNotNil: 
			[ encodedBodyText := (self decoderClass mimeEncode: encodedBodyText readStream) upToEnd ].
		str nextPutAll: encodedBodyText ]