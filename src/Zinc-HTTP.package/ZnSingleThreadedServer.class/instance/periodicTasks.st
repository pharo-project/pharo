private
periodicTasks
	"Every #acceptWaitTimeout seconds this method is called.
	Note that during this time the server is not actively listening for connections."
	
	sessionManager ifNotNil: [ sessionManager cleanupInvalidSessions ]