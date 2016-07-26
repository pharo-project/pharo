convenience
forEncoding: encodingString
	"Answer a converter class for the given encoding or nil if unknown"
	encodingString ifNil: [^nil].
	^ encodingString asLowercase caseOf: 
		{ ['base64'] -> [Base64MimeConverter].
		  ['quoted-printable'] -> [QuotedPrintableMimeConverter]}
		otherwise: [].
