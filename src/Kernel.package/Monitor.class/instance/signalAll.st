signaling-default
signalAll
	"All processes waiting for the default event are woken up."

	^ self signalAll: nil