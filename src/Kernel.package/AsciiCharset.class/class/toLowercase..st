casing
toLowercase: aCharacter
	"(AsciiCharset new toLowercase: $A) >>> $a.
	(AsciiCharset new toLowercase: $a) >>> $a.
	(AsciiCharset new toLowercase: $!) >>> $!"
	(aCharacter between: $A and: $Z)
		ifFalse: [ ^ aCharacter ].
	
	^ Character value: (aCharacter asciiValue + $a asInteger - $A asInteger )