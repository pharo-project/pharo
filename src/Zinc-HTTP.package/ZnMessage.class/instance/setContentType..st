accessing
setContentType: mimeType
	"Explicitly set my Content-Type to mimeType.
	I normally do this automatically by taking the #contentType from my entity."
	
	self headers contentType: mimeType