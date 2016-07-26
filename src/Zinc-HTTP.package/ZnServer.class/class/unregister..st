system startup
unregister: server
	"No longer send server start/stop on system startUp/shutDown"
	
	self managedServers remove: server ifAbsent: [ ]