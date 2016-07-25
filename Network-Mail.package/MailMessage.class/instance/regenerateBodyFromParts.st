printing/formatting
regenerateBodyFromParts
	"regenerate the message body from the multiple parts"
	| bodyText |

	bodyText := String streamContents: [ :str |
		str cr.
		parts do: [ :part |
			str
				cr;
				nextPutAll: '--';
				nextPutAll: self attachmentSeparator;
				cr;
				nextPutAll: part text ].
	
		str
			cr;
			nextPutAll: '--';
			nextPutAll: self attachmentSeparator;
			nextPutAll: '--';
			cr ].

	body := MIMEDocument contentType: 'multipart/mixed' content: bodyText.
	text := nil.  "text needs to be reformatted"