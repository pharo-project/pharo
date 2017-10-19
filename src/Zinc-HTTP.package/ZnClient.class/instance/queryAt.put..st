accessing request
queryAt: key put: value
	"Add key equals value as an HTTP query parameter to the current request.
	If the key already exists, its value is overwritten."
	
	request url 
		ifNil: [ request url: (ZnUrl new queryAt: key put: value; yourself) ]
		ifNotNil: [ request url queryAt: key put: value ]  