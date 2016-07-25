accessing request
contentType: mimeType
	"Set the content-type of the request entity, provided we have one"
	
	request hasEntity
		ifTrue: [ 
			request entity contentType: mimeType asZnMimeType ]
		ifFalse: [ 
			self error: 'There is no request entity yet, cannot set its content-type' ]