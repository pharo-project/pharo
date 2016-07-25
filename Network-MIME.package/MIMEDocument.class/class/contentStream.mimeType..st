instance creation
contentStream: aStream mimeType: aMimeType 
	"create a MIMEDocument with the given content-type and contentStream"
	"MIMEDocument mimeType: 'text/plain' asMIMEType contentStream: (ReadStream on: 'This is a test')"
	
	^self contentStream: aStream mimeType: aMimeType uri: aStream uri