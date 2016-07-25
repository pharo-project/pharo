private
digitLshift: shiftCount 
	| carry rShift mask len result digit byteShift bitShift highBit |
	(highBit := self highBitOfMagnitude) = 0 ifTrue: [^ 0].
	len := highBit + shiftCount + 7 // 8.
	result := Integer new: len neg: self negative.
	byteShift := shiftCount // 8.
	bitShift := shiftCount \\ 8.
	bitShift = 0 ifTrue: ["Fast version for byte-aligned shifts"
		^ result
			replaceFrom: byteShift + 1
			to: len
			with: self
			startingAt: 1].
	carry := 0.
	rShift := bitShift - 8.
	mask := 255 bitShift: 0 - bitShift.
	1 to: byteShift do: [:i | result digitAt: i put: 0].
	1 to: len - byteShift do: 
		[:i | 
		digit := self digitAt: i.
		result digitAt: i + byteShift put: (((digit bitAnd: mask)
				bitShift: bitShift)
				bitOr: carry).
		carry := digit bitShift: rShift].
	^ result