OS signal sending
sigchld
	"Send a SIGCHLD signal to the external process represented by self"

	^ self systemAccessor kill: self pid signal: SIGCHLD