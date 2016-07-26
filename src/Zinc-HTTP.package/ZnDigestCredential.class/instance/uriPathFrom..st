accessing
uriPathFrom: urlObject
	^ urlObject pathPrintString ifNil: [ '/' ]