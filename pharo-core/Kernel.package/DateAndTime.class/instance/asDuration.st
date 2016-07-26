converting
asDuration
	"Answer the duration since midnight."

	^ Duration seconds: self secondsSinceMidnightLocalTime nanoSeconds: nanos
