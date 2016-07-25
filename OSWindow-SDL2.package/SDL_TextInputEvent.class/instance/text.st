accesing
text
	| len text |
	len := self strlen.
	text := ByteArray new: len.
	1 to: len do: [ :i |
		text at: i put: (self getHandle nbUInt8AtOffset: 12 + i - 1)
	].
	^ text