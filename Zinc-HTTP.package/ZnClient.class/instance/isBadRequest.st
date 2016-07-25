testing
isBadRequest
	"Return true if the last executed HTTP request returned an HTTP bad request response code."
	
	^ response 
		ifNil: [ self error: 'Request not yet executed' ]
		ifNotNil: [ response isBadRequest ]