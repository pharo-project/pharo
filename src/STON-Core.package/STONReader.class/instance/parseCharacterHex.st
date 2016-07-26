private
parseCharacterHex
	| value codePoint |
	value := self parseCharacterHex4Value.
	(value < 16rD800 or: [ value > 16rDBFF ])
		ifTrue: [ codePoint := value ]
		ifFalse: [ | leadSurrogate trailSurrogate |
			"Characters not in the Basic Multilingual Plane are encoded as a UTF-16 surrogate pair"
			"See https://tools.ietf.org/html/rfc7159#section-7"
			leadSurrogate := value.
			trailSurrogate := self parseTrailingSurrogateHexEscape.
			codePoint := (leadSurrogate - 16rD800) * 16r400 + (trailSurrogate - 16rDC00).
			codePoint := 16r10000 + codePoint ].
	^ Character codePoint: codePoint