clipboard
strlen: text
	| res |
	res := 0.
	[ (text nbUInt8AtOffset: res) ~= 0 ] whileTrue:
		[ res := res + 1 ].
	^ res