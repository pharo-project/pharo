multipart
makeMultipart: subType with: newPart
	"if I am not multipart already, then become a multipart message with one part"

	| multipartHeader |

	body isMultipart
		ifFalse: [
			parts := Array with: newPart.

			"fix up our header"
			multipartHeader := MIMEHeaderValue fromMIMEHeader: 'multipart/',subType.
			multipartHeader parameterAt: 'boundary' put: self class generateSeparator .
			self setField: 'content-type' to: multipartHeader.

			self setField: 'mime-version' to: (MIMEHeaderValue fromMIMEHeader: '1.0').
			self removeFieldNamed: 'content-transfer-encoding']
		ifTrue: [
			self parts.
			parts := parts copyWith: newPart.
			].

	"regenerate everything"
	self regenerateBodyFromParts.
	text := nil.