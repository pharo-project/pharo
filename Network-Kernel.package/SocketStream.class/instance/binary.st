configuration
binary
	"Tell the SocketStream to send data
	as ByteArrays instead of Strings.
	Default is ascii."

	binary := true.
	inBuffer
		ifNil: [self resetBuffers]
		ifNotNil:
			[inBuffer := inBuffer asByteArray.
			outBuffer := outBuffer asByteArray]