other
hex: aFloat
	"Return an hexadecimal two-digits string between 00 and FF
	for a float between 0.0 and 1.0"
	| str |
	str := ((aFloat * 255) asInteger printStringHex) asLowercase.
	str size = 1 ifTrue: [^'0',str] ifFalse: [^str]