configuration
ascii
	"Tell the SocketStream to send data
	as Strings instead of ByteArrays.
	This is default."

	binary := false.
	inBuffer
		ifNil: [self resetBuffers]
		ifNotNil:
			[inBuffer := inBuffer asString.
			outBuffer := outBuffer asString]