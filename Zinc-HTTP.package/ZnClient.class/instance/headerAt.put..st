accessing request
headerAt: key put: value
	"Set key equals value in the HTTP header of the current request"
	
	request headers 
		at: key put: value   