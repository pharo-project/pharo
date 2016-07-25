testing
isRedirect
	"Return true it the last executed HTTP request returned an HTTP redirect response code."
	
	^ response 
		ifNil: [ self error: 'Request not yet executed' ]
		ifNotNil: [ response isRedirect ]