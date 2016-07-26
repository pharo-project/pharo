OS signal sending
sigusr2
	"Send a SIGUSR2 signal to the external process represented by self"

	^ self systemAccessor kill: self pid signal: SIGUSR2