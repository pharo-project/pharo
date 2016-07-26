OS signal sending
sighup
	"Send a SIGHUP signal to the external process represented by self"

	^ self systemAccessor kill: self pid signal: SIGHUP