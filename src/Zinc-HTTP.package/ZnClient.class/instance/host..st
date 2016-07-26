accessing request
host: hostName
	"Set the host of the current request to hostName."
	
	self url: (ZnUrl new host: hostName; yourself)