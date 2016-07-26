stream access
nextLong
	"Read a 32-bit quantity from the input stream."

	^(stream next bitShift: 24) + (stream next bitShift: 16) +
		(stream next bitShift: 8) + stream next