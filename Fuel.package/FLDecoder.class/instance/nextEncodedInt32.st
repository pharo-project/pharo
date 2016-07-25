decoding
nextEncodedInt32
	| n firstDigit |
	n := firstDigit := stream next.
	n := (n bitShift: 8) + stream next.
	n := (n bitShift: 8) + stream next.
	n := (n bitShift: 8) + stream next.
	firstDigit >= 128 ifTrue: [n := -16r100000000 + n].  "decode negative 32-bit integer"
	^ n
