OS signal sending
sigalrm
	"Send a SIGALRM signal to the external process represented by self"

	^ self systemAccessor kill: self pid signal: SIGALRM