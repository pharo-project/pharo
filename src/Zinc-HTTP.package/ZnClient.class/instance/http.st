accessing request
http
	"Set the scheme of the current request to HTTP."
	
	self url: (ZnUrl new scheme: #http; yourself)