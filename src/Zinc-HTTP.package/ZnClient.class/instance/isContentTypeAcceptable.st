testing
isContentTypeAcceptable
	"Return true if the contentType of the response of the last executed 
	HTTP request matches the accept header of the request."
	
	^ response 
		ifNil: [ self error: 'Request not yet executed' ]
		ifNotNil: [ 
			response hasEntity not 
				or: [ response contentType matches: request accept asZnMimeType ] ]