instance creation
contents: content mimeType: aMimeType uri: aURL
	"create a MIMEDocument with the given content-type and content"
	"MIMEDocument mimeType: 'text/plain' asMIMEType content: 'This is a test'"
	
	^self new contents: content mimeType: aMimeType uri: aURL