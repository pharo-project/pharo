OS signal sending
sigkill
	"Send a SIGKILL signal to the external process represented by self"

	^ self systemAccessor kill: self pid signal: SIGKILL