system startup
register: server
	"Arrange for server to be sent start/stop on system startUp/shutDown"
	
	self managedServers add: server