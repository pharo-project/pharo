OS signal sending
sigterm
	"Send a SIGTERM signal to the external process represented by self"

	^ self systemAccessor kill: self pid signal: SIGTERM