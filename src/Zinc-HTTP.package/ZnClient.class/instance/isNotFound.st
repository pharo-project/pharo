testing
isNotFound
	"Return true it the last executed HTTP request returned an HTTP not found response code."
	
	^ response 
		ifNil: [ self error: 'Request not yet executed' ]
		ifNotNil: [ response isNotFound ]