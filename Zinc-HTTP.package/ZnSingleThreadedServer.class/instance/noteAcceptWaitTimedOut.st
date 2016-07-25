private
noteAcceptWaitTimedOut
	"The accept wait on a server socket timed out (see #acceptWaitTimeout).
	This can be used as an opportunity to do some periodic tasks."
	
	self periodicTasks