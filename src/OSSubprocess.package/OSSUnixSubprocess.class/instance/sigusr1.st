OS signal sending
sigusr1
	"Send a SIGUSR1 signal to the external process represented by self"

	^ self systemAccessor kill: self pid signal: SIGUSR1