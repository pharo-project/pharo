instance creation
forField: aFName fromString: aString
	"Create a MIMEHeaderValue from aString.  How it is parsed depends on whether it is a MIME specific field or a generic header field."

	(aFName beginsWith: 'content-') 
		ifTrue: [^self fromMIMEHeader: aString]
		ifFalse: [^self fromTraditionalHeader: aString]
