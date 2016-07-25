private
initPortNumber: anInteger queueLength: queueLength
	"Private! Initialize the receiver to listen on the given port number. Up to queueLength connections will be queued."

	portNumber := anInteger.
	maxQueueLength := queueLength.
	connections := OrderedCollection new.
	accessSema := Semaphore forMutualExclusion.
	socket := nil.
	process := [self listenLoop] newProcess.
	process priority: Processor highIOPriority.
	process resume.
