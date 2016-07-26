accessing request
port: portNumber
	"Set the port of the current request to portNumber."
	
	self url: (ZnUrl new port: portNumber; yourself)