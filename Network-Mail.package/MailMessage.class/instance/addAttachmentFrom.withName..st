multipart
addAttachmentFrom: aStream withName: aName
	"add an attachment, encoding with base64.  aName is the option filename to encode"
	| newPart |
	self makeMultipart.
	self parts.  "make sure parts have been parsed"

	"create the attachment as a MailMessage"
	newPart := MailMessage empty.
	newPart setField: 'content-type' toString: 'application/octet-stream'.
	newPart setField: 'content-transfer-encoding' toString: 'base64'.
	aName ifNotNil: [
		| dispositionField |
		dispositionField := MIMEHeaderValue fromMIMEHeader: 'attachment'.
		dispositionField parameterAt: 'filename' put: aName.
		newPart setField: 'content-disposition' to: dispositionField ].
	newPart body: (MIMEDocument contentType: 'application/octet-stream' content: aStream upToEnd).


	"regenerate our text"
	parts := parts copyWith: newPart.
	self regenerateBodyFromParts.
	text := nil.