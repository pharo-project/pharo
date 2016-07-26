compatibility
contentType: aMIMEType content: content
	"create a MIMEDocument with the given content-type and content"
	"MIMEDocument contentType: 'text/plain' content: 'This is a test'"
	
	^ self new
		privateContent: content;
		type: aMIMEType asZnMimeType;
		yourself