public
stop: unregister
	super stop: unregister.
	self closeConnections