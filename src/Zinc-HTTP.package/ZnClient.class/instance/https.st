accessing request
https
	"Set the scheme of the current request to HTTPS."
	
	self url: (ZnUrl new scheme: #https; yourself)