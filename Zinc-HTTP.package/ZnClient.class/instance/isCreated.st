testing
isCreated
	"Return true it the last executed HTTP request returned an HTTP created response code."
	
	^ response 
		ifNil: [ self error: 'Request not yet executed' ]
		ifNotNil: [ response isCreated ]