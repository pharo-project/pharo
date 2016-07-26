OS signal sending
sigabrt
	"Send a SIGABRT signal to the external process represented by self"

	^ self systemAccessor kill: self pid signal: SIGABRT