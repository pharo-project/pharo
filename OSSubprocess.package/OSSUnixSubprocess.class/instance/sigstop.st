OS signal sending
sigstop
	"Send a SIGSTOP signal to the external process represented by self"

	^ self systemAccessor kill: self pid signal: SIGSTOP