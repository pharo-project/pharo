private-bits access
nextBits
	| integer readBitCount shiftCount byte |
	integer := 0.
	remainBitCount = 0 
		ifTrue: 
			[ readBitCount := 8.
			shiftCount := 0 ]
		ifFalse: 
			[ readBitCount := remainBitCount.
			shiftCount := remainBitCount - 8 ].
	[ readBitCount < codeSize ] whileTrue: 
		[ byte := self nextByte.
		byte == nil ifTrue: [ ^ eoiCode ].
		integer := integer + (byte bitShift: shiftCount).
		shiftCount := shiftCount + 8.
		readBitCount := readBitCount + 8 ].
	(remainBitCount := readBitCount - codeSize) = 0 
		ifTrue: [ byte := self nextByte ]
		ifFalse: [ byte := self peekByte ].
	byte == nil ifTrue: [ ^ eoiCode ].
	^ integer + (byte bitShift: shiftCount) bitAnd: maxCode