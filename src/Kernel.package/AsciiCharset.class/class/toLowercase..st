casing
toLowercase: aCharacter
	"(AsciiCharset toLowercase: $A) >>> $a.
	 (AsciiCharset  toLowercase: $a) >>> $a.
	 (AsciiCharset  toLowercase: $!) >>> $!"

	(aCharacter between: $A and: $Z)
		ifFalse: [ ^ aCharacter ].
	^ Character value: aCharacter asciiValue + $a asInteger - $A asInteger