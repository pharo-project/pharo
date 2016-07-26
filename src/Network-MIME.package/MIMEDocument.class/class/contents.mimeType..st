instance creation
contents: content mimeType: aMimeType 
	"create a MIMEDocument with the given content-type and content"
	"MIMEDocument mimeType: 'text/plain' asMIMEType content: 'This is a test'"
	
	^self contents: content mimeType: aMimeType uri: nil