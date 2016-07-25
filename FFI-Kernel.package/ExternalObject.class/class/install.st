system startup
install
	"Notify all instances of the receiver that we're coming up on a new platform.
	Note: The default implementation does nothing since the general external
	objects are cleaned up by ExternalAddress>>startUp: but subclasses may
	implement this method so that the appropriate action for existing instances can
	be taken."