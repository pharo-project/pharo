multipart
makeMultipart
	"if I am not multipart already, then become a multipart message with one part"

	| part multipartHeader |

	body isMultipart ifTrue: [ ^self ].

	"set up the new message part"
	part := MailMessage empty.
	part body: body.
	(self hasFieldNamed: 'content-type') ifTrue: [
		part setField: 'content-type' to: (self fieldNamed: 'content-type' ifAbsent: ['']) ].
	parts := Array with: part.

	"fix up our header"
	multipartHeader := MIMEHeaderValue fromMIMEHeader: 'multipart/mixed'.
	multipartHeader parameterAt: 'boundary' put: self class generateSeparator .
	self setField: 'content-type' to: multipartHeader.

	self setField: 'mime-version' to: (MIMEHeaderValue fromMIMEHeader: '1.0').
	self removeFieldNamed: 'content-transfer-encoding'.

	"regenerate everything"
	self regenerateBodyFromParts.
	text := nil.