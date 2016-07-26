decoding
nextEncodedInt24
	| n firstDigit |
	n := firstDigit := stream next.
	n := (n bitShift: 8) + stream next.
	n := (n bitShift: 8) + stream next.
	firstDigit >= 128 ifTrue: [n := -16r1000000 + n].  "decode negative 24-bit integer"
	^ n
