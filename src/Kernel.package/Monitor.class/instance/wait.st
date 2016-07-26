waiting-basic
wait
	"Unconditional waiting for the default event.
	The current process gets blocked and leaves the monitor, which means that the monitor
	allows another process to execute critical code. When the default event is signaled, the
	original process is resumed."

	^ self waitMaxMilliseconds: nil