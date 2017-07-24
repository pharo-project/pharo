casing
toUppercase: aCharacter
	"(AsciiCharset new toUppercase: $a) >>> $A.
	(AsciiCharset new toUppercase: $A) >>> $A.
	(AsciiCharset new toUppercase: $!) >>> $!"
	(aCharacter between: $a and: $z)
		ifFalse: [ ^ aCharacter ].
	
	^ Character value: (aCharacter asciiValue - $a asInteger + $A asInteger )