testing
isSuccess 
	"Return true it the last executed HTTP request returned an HTTP success response code."
	
	^ response 
		ifNil: [ self error: 'Request not yet executed' ]
		ifNotNil: [ response isSuccess ]