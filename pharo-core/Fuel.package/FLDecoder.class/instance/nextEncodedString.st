decoding
nextEncodedString
	| length aByteArray |

	"read the length in binary mode"
	length := stream next.		"first byte."
	length >= 192 ifTrue: [length := length - 192.
		1 to: 3 do: [:ii | length := length * 256 + stream next]].
	aByteArray := ByteArray new: length.

	stream nextInto: aByteArray.
	^aByteArray asString.
