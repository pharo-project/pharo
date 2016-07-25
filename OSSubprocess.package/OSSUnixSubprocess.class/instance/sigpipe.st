OS signal sending
sigpipe
	"Send a SIGPIPE signal to the external process represented by self"

	^ self systemAccessor kill: self pid signal: SIGPIPE