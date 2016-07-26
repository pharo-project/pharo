system startup
installSubclasses
	"Notify all the subclasses of ExternalObject that we are starting up on a new platform."
	self withAllSubclassesDo:[:cls| cls install].