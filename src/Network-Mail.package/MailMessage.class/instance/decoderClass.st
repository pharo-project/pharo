multipart
decoderClass
	| encoding |
	encoding := self fieldNamed: 'content-transfer-encoding' ifAbsent: [^ nil].
	encoding := encoding mainValue.
	encoding asLowercase = 'base64' ifTrue: [^ Base64MimeConverter].
	encoding asLowercase = 'quoted-printable' ifTrue: [^ QuotedPrintableMimeConverter].
	^ nil