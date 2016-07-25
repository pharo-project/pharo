accessing
initializeLatin1MapAndEncodings
	"Initialize the latin1Map and latin1Encodings.
	These variables ensure that conversions from latin1 ByteString is reasonably fast"
	
	| latin1 utf8 |
	latin1Map := ByteArray new: 256.
	latin1Encodings := Array new: 256.
	0 to: 255 do:[:i|
		utf8 := (String new: 8) writeStream.
		latin1 := String with: (Character value: i).
		self new nextPut: latin1 first toStream: utf8.
		utf8 := utf8 contents.
		latin1 = utf8 ifTrue:[
			latin1Map at: i+1 put: 0. "no translation needed"
		] ifFalse:[
			latin1Map at: i+1 put: 1. "translation needed"
			latin1Encodings at: i+1 put: utf8.
		].
	].