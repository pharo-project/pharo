converting
encodingLengthFor: byte
	byte < 128 ifTrue: [ ^ 1 ].
	(byte bitAnd: 2r11100000) == 2r11000000 ifTrue: [ ^ 2 ].
	(byte bitAnd: 2r11110000) == 2r11100000 ifTrue: [ ^ 3 ].
	(byte bitAnd: 2r11111000) == 2r11110000 ifTrue: [ ^ 4 ].
	self errorIllegalLeadingByte