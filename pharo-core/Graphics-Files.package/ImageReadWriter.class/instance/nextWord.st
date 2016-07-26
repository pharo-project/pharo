stream access
nextWord
	"Read a 16-bit quantity from the input stream."

	^(stream next bitShift: 8) + stream next