testing
isNotModified
	"Return true it the last executed HTTP request returned an HTTP not modified response code."
	
	^ response 
		ifNil: [ self error: 'Request not yet executed' ]
		ifNotNil: [ response isNotModified ]