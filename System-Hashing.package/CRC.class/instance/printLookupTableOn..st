private-tables
printLookupTableOn: aStream
	| topBit |
	topBit := self bitMaskAt: width.
	0 to: 255 do: [ :index || register indexByte |
		indexByte := index.
		shouldReflectInput ifTrue: [ indexByte := self reflect: indexByte onLowerBits: 8 ].
		register := indexByte << self lowestByteShift.
		1 to: 8 do: [ : byteIndex |
			register :=
				(register bitAnd: topBit) > 0 
					ifTrue: [ (register << 1) bitXor: polynome ]
					ifFalse: [ register << 1 ] ].
		shouldReflectInput ifTrue: [ 
			register := self reflect: register onLowerBits: width ].
		register := (register bitAnd: self widthMask).
		aStream nextPut: register ]