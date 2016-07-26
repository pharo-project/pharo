instance creation
on: aStream
	"Open a new DataStream onto a low-level I/O stream."

	^ self basicNew setStream: aStream
		"aStream binary is in setStream:"
