instance creation
contentStream: aStream mimeType: aMimeType uri: aURI
	"create a MIMEDocument with the given content-type and contentStream"
	"MIMEDocument mimeType: 'text/plain' asMIMEType contentStream: (ReadStream on: 'This is a test')"
	
	^self new contentStream: aStream mimeType: aMimeType uri: aURI