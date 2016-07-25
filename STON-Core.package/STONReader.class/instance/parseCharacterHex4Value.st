private
parseCharacterHex4Value
	| value |
	value := self parseCharacterHexDigit.
	3 timesRepeat: [ 
		value := (value << 4) + self parseCharacterHexDigit ].
	^ value